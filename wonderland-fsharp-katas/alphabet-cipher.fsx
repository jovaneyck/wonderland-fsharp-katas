// See the file alphabet-cipher.md for detailed information.

type Message = string
type Keyword = string

let allLetters =
    [for c in int('a')..int('z') do
     yield char(c)]

let rec infinite alphabet = 
    seq {
        for l in alphabet do yield l
        for l in infinite alphabet do yield l
    }

let toIndex c = (int c) - (int 'a')
let fromIndex c = int('a') + c |> char

let lettersStartingWith letter =
    infinite allLetters
    |> Seq.skip (toIndex letter)

let encodeLetter secretletter letter =
    lettersStartingWith letter
    |> Seq.item (toIndex secretletter)

let decodeLetter letter secretLetter = 
    let (indexOfLetter, _) = 
        (lettersStartingWith secretLetter)
        |> Seq.indexed
        |> Seq.find (fun (idx, l) -> l = letter)
    indexOfLetter |> fromIndex

let translate lettertranslator key (message : Message) =
    let secret = infinite key |> Seq.take (message.Length)
    let secretAndMessage = secret |> Seq.zip message

    secretAndMessage
    |> Seq.map (fun (s,m) -> lettertranslator s m)
    |> Array.ofSeq
    |> Message

let encode : (Keyword -> Message -> Message) = translate encodeLetter

let decode : (Keyword -> Message -> Message) = translate decodeLetter

let crack msgLetter cipherLetter =
    let(indexOfCipher,_) =
        lettersStartingWith msgLetter
        |> Seq.indexed
        |> Seq.find (fun (_, l) -> l = cipherLetter)
    indexOfCipher |> fromIndex

let toString (charArray : char list) = System.String.Join("",charArray)
let startswith substring text = 
    (text |> toString).StartsWith(substring |> toString)

let rec isConstructedByRepeating (subset : char list) (text : char list) = 
    if text.Length <= subset.Length
    then subset |> startswith text
    else 
        text |> startswith subset 
        && text |> List.skip(subset.Length) |> isConstructedByRepeating subset

let smallestRepeatingSubset allchars = 
    let rec srs acc (chars : char list) = 
        match chars with
        | [] -> acc
        | h :: t -> 
            let subset = acc @ [h]
            if allchars |> isConstructedByRepeating subset
            then subset
            else srs subset t
    srs [] allchars

let decipher (cipher:Message) (message:Message) : Keyword =
    message
    |> Seq.zip cipher
    |> Seq.map (fun (c, m) -> crack m c)
    |> Seq.toList
    |> smallestRepeatingSubset
    |> Array.ofSeq
    |> Keyword

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()