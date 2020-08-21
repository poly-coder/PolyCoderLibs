module PolyCoder.System

open System.Text
open System
open System.Globalization
open System.Security.Cryptography

let toUtf8 (str: string) = Encoding.UTF8.GetBytes(str)
let ofUtf8 (bytes: byte[]) = Encoding.UTF8.GetString(bytes)

let fromBase64 (str: string) = Convert.FromBase64String(str)
let toBase64 (bytes: byte[]) = Convert.ToBase64String(bytes)

// TODO: Optimize this functions to work directly with chars instead of substrings
let toHexString (bytes: byte[]) =
    if bytes.Length = 0 then "" else
    let builder = StringBuilder(bytes.Length * 2)
    for i = 0 to bytes.Length - 1 do
        let str = bytes.[i].ToString("X2")
        builder.Append(str) |> ignore
    builder.ToString()

let toHexStringLower (bytes: byte[]) =
    if bytes.Length = 0 then "" else
    let builder = StringBuilder(bytes.Length * 2)
    for i = 0 to bytes.Length - 1 do
        let str = bytes.[i].ToString("x2")
        builder.Append(str) |> ignore
    builder.ToString()

let fromHexString (str: string) =
    if str.Length = 0 then
        Array.empty
    elif str.Length % 2 <> 0 then
        invalidOp "Hex string must have even amount of characters"
    else
        let bytes = Array.zeroCreate (str.Length / 2)
        for i = 0 to bytes.Length - 1 do
            bytes.[i] <- Byte.Parse(str.Substring(i * 2, 2), NumberStyles.HexNumber)
        bytes

let genRandomOn (bytes: byte[]) =
    use rng = new RNGCryptoServiceProvider()
    rng.GetBytes(bytes)

let genRandom size =
    let bytes = Array.zeroCreate size
    genRandomOn bytes
    bytes

let genNonZeroRandomOn (bytes: byte[]) =
    use rng = new RNGCryptoServiceProvider()
    rng.GetNonZeroBytes(bytes)

let genNonZeroRandom size =
    let bytes = Array.zeroCreate size
    genNonZeroRandomOn bytes
    bytes

let getSHA1Bytes (bytes: byte[]) =
    use hasher = new SHA1Managed()
    hasher.ComputeHash(bytes)

let getShortSHA1Bytes length (bytes: byte[]) =
    let hash = getSHA1Bytes bytes
    let response = Array.zeroCreate length
    Array.blit hash 0 response 0 length
    response

let stringToSha1 = toUtf8 >> getSHA1Bytes >> toHexStringLower
let stringToShortSha1 length = toUtf8 >> getShortSHA1Bytes length >> toHexStringLower


[<RequireQualifiedAccess>]
module String =
    open System.Text.RegularExpressions

    let join (separator: string) (source: string seq) =
        String.Join(separator, source)

    let prepend value (text: string) = value + text

    let removeSuffix suffix value =
        let vlen = String.length value
        let slen = String.length suffix
        if vlen <= slen || not(value.EndsWith(suffix, StringComparison.Ordinal)) then
            value
        else
            value.Substring(0, vlen - slen)

    let removePrefix prefix value =
        let vlen = String.length value
        let plen = String.length prefix
        if vlen <= plen || not(value.StartsWith(prefix, StringComparison.Ordinal)) then
            value
        else
            value.Substring(prefix.Length)

    let removeRegexGroup (groupName: string) (regex: Regex) value =
        let m = regex.Match(value)
        if not m.Success then value
        else
            let group = m.Groups.Item(groupName)
            if isNotNull group && group.Success
            then value.Remove(group.Index, group.Length)
            else value

    let removeRegex (regex: Regex) value =
        let m = regex.Match(value)
        if not m.Success then value
        else value.Remove(m.Index, m.Length)

    let removePatternGroup groupName (pattern: string) =
        let regex = Regex(pattern)
        removeRegexGroup groupName regex

    let removePattern (pattern: string) =
        let regex = Regex(pattern)
        removeRegex regex

    let private beforeAux index text =
        match index with
        | i when i <= 0 -> text
        | i when i >= String.length text -> text
        | i -> text.Substring(0, i)

    let private afterAux separatorLength index (text: String) =
        match index with
        | i when i <= 0 -> text
        | i -> text.Substring(i + separatorLength)

    let beforeWith (comparison: StringComparison) (separator: string) (text: string) =
        let index = text.IndexOf(separator, comparison)
        beforeAux index text

    let beforeLastWith (comparison: StringComparison) (separator: string) (text: string) =
        let index = text.LastIndexOf(separator, comparison)
        beforeAux index text

    let before = beforeWith StringComparison.Ordinal
    let beforeLast = beforeLastWith StringComparison.Ordinal

    let afterWith (comparison: StringComparison) (separator: string) (text: string) =
        let index = text.IndexOf(separator, comparison)
        afterAux (String.length separator) index text

    let afterLastWith (comparison: StringComparison) (separator: string) (text: string) =
        let index = text.LastIndexOf(separator, comparison)
        afterAux (String.length separator) index text

    let after = afterWith StringComparison.Ordinal
    let afterLast = afterLastWith StringComparison.Ordinal
    

module Reflection =
    open System.Linq
    open System.Reflection

    let private StandardTypesShortFormat =
        [
            typeof<string>, "string"
            typeof<char>, "char"
            typeof<bool>, "bool"
            typeof<uint8>, "byte"
            typeof<uint16>, "ushort"
            typeof<uint32>, "uint"
            typeof<uint64>, "ulong"
            typeof<int8>, "sbyte"
            typeof<int16>, "short"
            typeof<int32>, "int"
            typeof<int64>, "long"
            typeof<float32>, "float32"
            typeof<float>, "float"
            typeof<decimal>, "decimal"
            typeof<obj>, "obj"
            typeof<unit>, "unit"
            typeof<Void>, "void"
        ].ToDictionary(fst, snd)
    

    let rec formatTypeAs full (t: Type) =
        let rec loop (t: Type) =
            if isNull t then
                "null"
            elif full && StandardTypesShortFormat.ContainsKey(t) then
                StandardTypesShortFormat.Item(t)
            elif t.IsGenericParameter then
                t.Name
            elif t.IsArray then
                let elementType = t.GetElementType() |> loop
                let rank = t.GetArrayRank()
                let commas = String(',', rank - 1)
                sprintf "%s[%s]" elementType commas
            elif typeof<Delegate>.IsAssignableFrom(t) then
                let method = t.GetMethod("Invoke")
                method |> formatSignatureAs full (Some t.Name)
            else
                let typeName = if full then t.FullName else t.Name

                if t.IsGenericType then
                    let builder = StringBuilder()
                    let typeName = typeName |> String.before "`"
                    builder.Append(typeName).Append("<") |> ignore

                    let args = t.GetGenericArguments()
                    for i = 0 to args.Length - 1 do
                        if i > 0 then builder.Append(", ") |> ignore
                        let argName = args.[i] |> loop
                        builder.Append(argName) |> ignore
                
                    builder.Append(">") |> ignore
                    builder.ToString()
                else typeName

        loop t

    and formatSignatureAs full (methodName: string option) (method: MethodInfo) =
        let methodName =
            methodName
            |> Option.defaultWith (fun () ->
                if full then
                    sprintf "%s.%s" (formatTypeAs full method.DeclaringType) method.Name
                else method.Name
            )

        let builder = StringBuilder()

        builder.Append(formatTypeAs full method.ReturnType).Append(" ").Append(methodName).Append("(") |> ignore

        let parameters = method.GetParameters()

        for i = 0 to parameters.Length - 1 do
            if i > 0 then builder.Append(", ") |> ignore
            let p = parameters.[i]
            builder.Append(formatTypeAs full p.ParameterType).Append(" ").Append(p.Name) |> ignore

        builder.Append(")") |> ignore

        builder.ToString()

    let formatType t = formatTypeAs false t
    let formatFullType t = formatTypeAs true t

    let formatSignature t = formatSignatureAs false None t
    let formatFullSignature t = formatSignatureAs true None t
