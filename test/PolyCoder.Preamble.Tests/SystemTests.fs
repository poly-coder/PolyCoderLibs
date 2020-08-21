module PolyCoder.SystemTests

open System
open System.Text
open NUnit.Framework
open FsCheck
open FsCheck.NUnit
open Swensen.Unquote
open PolyCoder
open System.Security.Cryptography

[<SetUp>]
let Setup () = ()

[<Property>]
let ``System.toUtf8 should encode to UTF8`` (NonNull str) =
  test <@ toUtf8 str = Encoding.UTF8.GetBytes(str) @>

[<Property>]
let ``System.ofUtf8 should decode from UTF8`` (NonNull str) =
  test <@ ofUtf8(toUtf8 str) = str @>

[<Property>]
let ``System.toBase64 should encode to Base64`` (NonNull array) =
  test <@ toBase64(array) = Convert.ToBase64String(array) @>

[<Property>]
let ``System.fromBase64 should decode from Base64`` (NonNull array) =
  test <@ fromBase64(toBase64(array)) = array @>

[<Property>]
let ``System.toHexString should encode to Hex String`` (NonNull array) =
  test <@ toHexString(array) = BitConverter.ToString(array).Replace("-", "") @>

[<Property>]
let ``System.toHexStringLower should encode to Hex String in lowercase`` (NonNull array) =
  test <@ toHexStringLower(array) = BitConverter.ToString(array).Replace("-", "").ToLowerInvariant() @>

[<Property>]
let ``System.fromHexString should decode from Hex String`` (NonNull array) =
  test <@ fromHexString(toHexString(array)) = array @>

[<Property>]
let ``System.fromHexString should decode from Hex String in lowercase`` (NonNull array) =
  test <@ fromHexString(toHexStringLower(array)) = array @>

[<Property>]
let ``System.fromHexString should throw exception for non-even length strings`` (NonNull array) (extra: byte) =
  let exn = Assert.Throws<InvalidOperationException>(fun () ->
    let extraByte = extra.ToString("X2").Substring(0, 1)
    let encoded = toHexString(array) + extraByte
    fromHexString encoded |> ignore)
  test <@ exn.Message = "Hex string must have even amount of characters" @>

[<Property>]
let ``System.genRandomOn should fill the array with random values`` (PositiveInt size) =
  let array = Array.zeroCreate (size + 16) // To reduce the chance of getting all zeros
  genRandomOn array
  test <@ array |> Seq.exists (fun b -> b <> 0uy) @>

[<Property>]
let ``System.genRandom should return an array with random values`` (PositiveInt size) =
  let array = genRandom (size + 16)
  test <@ isNotNull array @>
  test <@ array.Length = size + 16 @>
  test <@ array |> Seq.exists (fun b -> b <> 0uy) @>

[<Property>]
let ``System.genNonZeroRandomOn should fill the array with non-zero random values`` (PositiveInt size) =
  let array = Array.zeroCreate size // To reduce the chance of getting all zeros
  genNonZeroRandomOn array
  test <@ array |> Seq.forall (fun b -> b <> 0uy) @>

[<Property>]
let ``System.genNonZeroRandom should return an array with non-zero random values`` (PositiveInt size) =
  let array = genNonZeroRandom size
  test <@ isNotNull array @>
  test <@ array.Length = size @>
  test <@ array |> Seq.forall (fun b -> b <> 0uy) @>

[<Property>]
let ``System.getSHA1Bytes should compute the SHA1 hash`` (NonNull array) =
  let actual = getSHA1Bytes array
  use hasher = new SHA1Managed()
  let expected = hasher.ComputeHash(array)
  test <@ actual = expected @>

[<Property>]
let ``System.getShortSHA1Bytes should compute the first bytes of the SHA1 hash`` (NonNull array) =
  let actual = getShortSHA1Bytes 8 array
  let expected = Array.sub (getSHA1Bytes array) 0 8
  test <@ actual = expected @>

[<Property>]
let ``System.stringToSha1 should compute the SHA1 hash`` (NonNull str) =
  let actual = stringToSha1 str
  let expected = str |> toUtf8 |> getSHA1Bytes |> toHexStringLower
  test <@ actual = expected @>

[<Property>]
let ``System.stringToShortSha1 should compute the SHA1 hash`` (NonNull str) =
  let actual = stringToShortSha1 8 str
  let expected = str |> toUtf8 |> getShortSHA1Bytes 8 |> toHexStringLower
  test <@ actual = expected @>

