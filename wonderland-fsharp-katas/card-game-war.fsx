// See the file card-game.md for detailed information.

type Suit =
    | Spades
    | Clubs
    | Diamonds
    | Hearts

type Rank =
    | Value of int
    | Jack
    | Queen
    | King
    | Ace

type Card = Rank * Suit
type Winner = 
    | First
    | Second

let higherSuit suit otherSuit = 
    match suit, otherSuit with
    | Clubs, Spades -> First
    | Diamonds, Clubs -> First
    | Hearts, Diamonds -> First
    | _ -> Second

let higherRank rank otherRank = 
    match rank, otherRank with
    | Ace, _ -> First
    | _, Ace -> Second
    | King, _ -> First
    | _, King -> Second
    | Queen, _ -> First
    | _, Queen -> Second
    | Jack, _ -> First
    | _, Jack -> Second
    | _ -> failwith (sprintf "Unexpected case: %A - %A" rank otherRank)

let playRound (card1:Card,card2:Card) =
    match card1, card2 with
    | (Value v1, _), (Value v2, _) when v1 > v2 -> First
    | (Value v1, _), (Value v2, _) when v1 < v2 -> Second
    | (Value _, s1), (Value _, s2)              -> higherSuit s1 s2
    | _            , (Value _, _)               -> First
    | (Value _,  _), _                          -> Second
    | (r1, s1),      (r2, s2)      when r1 = r2 -> higherSuit s1 s2
    | (r1, _),       (r2, _)                    -> higherRank r1 r2

let rec playGame (hand1:Card list, hand2:Card list) =
    match (hand1, hand2) with
    | ([], _) -> Second
    | (_, []) -> First
    | (c1::t1, c2::t2) -> 
        let roundWinner = playRound (c1,c2)
        let cardsOnTable = [c1; c2]
        match roundWinner with
        | First -> playGame (t1 @ cardsOnTable, t2)
        | Second -> playGame (t1, t2 @ cardsOnTable)

#r "../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

// fill in tests for your game
let tests () =

    // playRound
    printfn "the highest rank wins the cards in the round"
    test <@ playRound ((Value 3, Hearts), (Value 4, Hearts)) = Second @>
    test <@ playRound ((Value 3, Hearts), (Value 4, Diamonds)) = Second @>
    test <@ playRound ((Value 4, Hearts), (Value 3, Hearts)) = First @>
    printfn "figures are higher rank than numbered values"
    test <@ playRound ((Value 4, Hearts), (King, Hearts)) = Second @>
    test <@ playRound ((Queen, Hearts), (Value 2, Hearts)) = First @>
    printfn "queens are higher rank than jacks"
    test <@ playRound ((Queen, Spades), (Jack, Spades)) = First @>
    test <@ playRound ((Queen, Spades), (Jack, Hearts)) = First @>
    test <@ playRound ((Jack, Spades), (Queen, Hearts)) = Second @>
    printfn "kings are higher rank than queens"
    test <@ playRound ((King, Spades), (Queen, Spades)) = First @>
    test <@ playRound ((Queen, Spades), (King, Hearts)) = Second @>
    printfn "aces are higher rank than kings"
    test <@ playRound ((Ace, Spades), (King, Spades)) = First @>
    test <@ playRound ((King, Spades), (Ace, Hearts)) = Second @>
    printfn "Figures can be several higher and still win"
    test <@ playRound ((Jack, Spades), (Ace, Hearts)) = Second @>
    printfn "if the ranks are equal, clubs beat spades"
    test <@ playRound ((Value 2, Clubs), (Value 2, Spades)) = First @>
    test <@ playRound ((Value 2, Spades), (Value 2, Clubs)) = Second @>
    test <@ playRound ((King, Spades), (King, Clubs)) = Second @>
    printfn "if the ranks are equal, diamonds beat clubs"
    test <@ playRound ((Value 2, Diamonds), (Value 2, Clubs)) = First @>
    test <@ playRound ((Value 2, Clubs), (Value 2, Diamonds)) = Second @>
    test <@ playRound ((Queen, Clubs), (Queen, Diamonds)) = Second @>
    printfn "if the ranks are equal, hearts beat diamonds"
    test <@ playRound ((Value 2, Hearts), (Value 2, Diamonds)) = First @>
    test <@ playRound ((Value 2, Diamonds), (Value 2, Hearts)) = Second @>

    // playGame
    printfn "the player loses when they run out of cards"
    test <@ playGame ([(Value 2, Hearts)], [])  = First @>
    test <@ playGame ([], [(Value 3, Hearts)])  = Second @>
    test <@ playGame ([(Value 2, Hearts)], [(Value 3, Hearts)])  = Second @>
    test <@ playGame ([(Value 2, Hearts); (Value 5, Hearts)], [(Value 3, Hearts); (Value 4, Hearts)])  = First @>
    
// run the tests
tests ()

//Let's do some exploratory testing!

let deck = 
    [
        for s in [Spades;Hearts;Clubs;Diamonds] do
            for v in 2..10 do 
                yield (Value v, s)
            yield (Jack, s)
            yield (Queen, s)
            yield (King, s)
            yield (Ace, s)
    ]

let rec insertAt index el list =
    match index, list with
    | (0, _) -> el :: list
    | (idx, h :: t) -> h :: (insertAt (idx - 1) el t)
    | _ -> failwith "insertAt called with out of bounds index"

let shuffle list rng =
    let rec shuffleRec a l = 
        match l with
        | [] -> a
        | h :: t -> 
            let index = rng((a |> List.length) + 1)
            let newAcc = a |> insertAt index h
            shuffleRec newAcc t

    shuffleRec [] list

let rng = new System.Random(1337)
let randomnumber = fun max -> rng.Next(max)

let shuffled = shuffle deck randomnumber
let (d1, d2) = shuffled |> List.indexed |> List.partition (fun (i,_) -> i % 2 = 0)
let deck1 = d1 |> List.map(fun (_, c) -> c)
let deck2 = d2 |> List.map(fun (_, c) -> c)

printfn "Let's play an actual game now!"
let winner = playGame (deck1, deck2)
printfn "%A won" winner