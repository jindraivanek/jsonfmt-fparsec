module Tests


open Expecto
open FParsecJsonFmt

[<Tests>]
let tests =
  testList "samples" [
    testCase "example1" <| fun _ ->
      let example1 = """{ "name" : "Scott", "isMale" : true, "bday" : {"year":2001, "month":12, "day":25 }, "favouriteColors" : ["blue", "green"] }"""
      let result = Json.parseToOption Json.FmtOptions.Default example1
      let expected = """{
  "name" : "Scott",
  "isMale" : true,
  "bday" : {
    "year" : 2001,
    "month" : 12,
    "day" : 25
  },
  "favouriteColors" : [
    "blue",
    "green"
  ]
}"""
      Expect.equal result (Some expected) ""
  ]
