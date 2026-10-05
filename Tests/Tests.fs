module Tests

open System
open Xunit


[<Fact>]
let ``2025 day 01 part 1 example`` () =
    Assert.Equal(Day1Year2025.part1 Day1Year2025Inputs.example, 3)

[<Fact>]
let ``2025 day 01 part 1`` () =
    Assert.Equal(Day1Year2025.part1 Day1Year2025Inputs.data, 1031)

[<Fact>]
let ``2025 day 01 part 2 example`` () =
    Assert.Equal(Day1Year2025.part2 Day1Year2025Inputs.example, 6)

[<Fact>]
let ``2025 day 01 part 2 wraparound`` () =
    Assert.Equal(Day1Year2025.part2 Day1Year2025Inputs.wraparound, 10)

[<Fact>]
let ``2025 day 01 part 2`` () =
    Assert.Equal(Day1Year2025.part2 Day1Year2025Inputs.data, 5831)
    
[<Fact>]
let ``2025 day 02 part 1`` () =
    Assert.Equal(Day2Year2025.part1 Day2Year2025Inputs.data, 28844599675I)

[<Fact>]
let ``2025 day 02 part 2`` () =
    Assert.Equal(Day2Year2025.part2 Day2Year2025Inputs.data, 48778605167I)
    
    
[<Fact>]
let ``2025 day 03 part 1 example`` () =
    Assert.Equal(Day3Year2025.part1 Day3Year2025Inputs.example, 357)

[<Fact>]
let ``2025 day 03 part 1`` () =
    Assert.Equal(Day3Year2025.part1 Day3Year2025Inputs.data, 17535)

[<Fact>]
let ``2025 day 03 part 2 example`` () =
    Assert.Equal(Day3Year2025.part2 Day3Year2025Inputs.example, 3121910778619I)

[<Fact(Skip="takes too long")>]
let ``2025 day 03 part 2`` () =
    Assert.Equal(Day3Year2025.part2 Day3Year2025Inputs.data, 173577199527257I)

[<Fact>]
let ``2025 day 04 part 1 example`` () =
    Assert.Equal(Day4Year2025.part1 Day4Year2025Inputs.example, 13)

[<Fact>]
let ``2025 day 04 part 1`` () =
    Assert.Equal(Day4Year2025.part1 Day4Year2025Inputs.data, 1344)

[<Fact>]
let ``2025 day 04 part 2 example`` () =
    Assert.Equal(Day4Year2025.part2 Day4Year2025Inputs.example, 43)

[<Fact>]
let ``2025 day 04 part 2`` () =
    Assert.Equal(Day4Year2025.part2 Day4Year2025Inputs.data, 8112)

[<Fact>]
let ``2025 day 05 part 1 example`` () =
    Assert.Equal(Day5Year2025.part1 Day5Year2025Inputs.example, 3)

[<Fact>]
let ``2025 day 05 part 1 edge cases`` () =
    Assert.Equal(Day5Year2025.part1 Day5Year2025Inputs.edgeCases, 1)

[<Fact>]
let ``2025 day 05 part 1`` () =
    Assert.Equal(Day5Year2025.part1 Day5Year2025Inputs.data, 511)

[<Fact>]
let ``2025 day 05 part 2 example`` () =
    Assert.Equal(Day5Year2025.part2 Day5Year2025Inputs.example, 14I)

[<Fact>]
let ``2025 day 05 part 2 edge cases`` () =
    Assert.Equal(Day5Year2025.part2 Day5Year2025Inputs.edgeCases, 101)

[<Fact>]
let ``2025 day 05 part 2`` () =
    Assert.Equal(Day5Year2025.part2 Day5Year2025Inputs.data, 350939902751909I)

[<Fact>]
let ``2025 day 06 part 1 example`` () =
    Assert.Equal(Day6Year2025.part1 Day6Year2025Inputs.example, 4277556I)

[<Fact>]
let ``2025 day 06 part 1`` () =
    Assert.Equal(Day6Year2025.part1 Day6Year2025Inputs.data, 5316572080628I)

[<Fact>]
let ``2025 day 06 part 2 example`` () =
    Assert.Equal(Day6Year2025.part2 Day6Year2025Inputs.example, 3263827I)

[<Fact>]
let ``2025 day 06 part 2`` () =
    Assert.Equal(Day6Year2025.part2 Day6Year2025Inputs.data, 11299263623062I)


[<Fact>]
let ``2025 day 07 part 1 input validation`` () =
    Assert.Throws<Exception>(fun()->Day7Year2025.part1 Day7Year2025Inputs.inputValidation:>obj)

[<Fact>]
let ``2025 day 07 part 1 example`` () =
    Assert.Equal(Day7Year2025.part1 Day7Year2025Inputs.example, 21)

[<Fact>]
let ``2025 day 07 part 1`` () =
    Assert.Equal(Day7Year2025.part1 Day7Year2025Inputs.data, 1678)

[<Fact>]
let ``2025 day 07 part 2 example`` () =
    Assert.Equal(Day7Year2025.part2 Day7Year2025Inputs.example, 40I)

[<Fact>]
let ``2025 day 07 part 2`` () =
    Assert.Equal(Day7Year2025.part2 Day7Year2025Inputs.data, 357525737893560I)

[<Fact>]
let ``2025 day 08 part 1 input validation`` () =
    Assert.Throws<Exception>(fun ()-> Day8Year2025.part1 Day8Year2025Inputs.inputValidation 1000:>obj)
