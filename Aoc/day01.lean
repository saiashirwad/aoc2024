import Std.Data.HashMap
open Std

def parseLine (line : String) : (Int × Int) :=
  match line.splitOn " " with
  | [a, b] => (a.toInt!, b.toInt!)
  | _      => panic! "_-_"

def part1 (fileStr : String) : Int :=
  let (nums1, nums2) := fileStr.splitOn "\n"
    |>.map parseLine
    |>.unzip
  List.zip (nums1.mergeSort) (nums2.mergeSort)
    |>.map (fun (a, b) => (a - b).natAbs)
    |>.foldl (· + ·) 0

def part2 (fileStr : String) := Id.run do
  let mut freq := (HashMap.empty : HashMap Int Int)
  let mut similarity : Int := 0

  let lines := fileStr.splitOn "\n" |>.map parseLine
  for line in lines do
    let (_, r) := line
    match freq.get? r with
    | some v => freq := freq.insert r (v + 1)
    | none   => freq := freq.insert r 1
  for line in lines do
    let (l, _) := line
    let occurrences := freq.get! l
    similarity := similarity + (occurrences * l)
  similarity
