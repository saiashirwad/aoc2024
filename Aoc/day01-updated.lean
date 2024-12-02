import Std.Data.HashMap
open Std

def abs (a : Int) : Int := if a < 0 then -a else a

def parseLine (line : String) : Option (Int × Int) :=
  match line.splitOn "   " with
  | [a, b]  => some (a.toInt!, b.toInt!)
  | _       => pure (0, 0)

def part1 (fileStr : String) : Option Int := do
  let lines := fileStr.splitOn "\n"
  let nums ← lines.mapM parseLine
  let (nums1, nums2) := nums.unzip
  pure (List.zip nums1.mergeSort nums2.mergeSort
    |>.map (fun (a, b) => abs (a - b))
    |>.foldl (· + ·) 0)

def part2 (fileStr : String) : Option Int := do
  let mut freq := (HashMap.empty : HashMap Int Int)
  let mut similarity : Int := 0

  let lines ← fileStr.splitOn "\n" |>.mapM parseLine
  for line in lines do
    let (_, r) := line
    match freq.get? r with
    | some v => freq := freq.insert r (v + 1)
    | none   => freq := freq.insert r 1

  for line in lines do
    let (l, _) := line
    let occurrences := freq.get? l
    match occurrences with
    | some v => similarity := similarity + v * l
    | none   => pure ()
  pure similarity

#eval do
  let file ← IO.FS.readFile "inputs/day1.txt"
  pure (part1 file)

#eval do
  let file ← IO.FS.readFile "inputs/day1.txt"
  pure (part2 file)
