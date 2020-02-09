my second Haskell program

for my second Haskell program, I decided to reimplement my [first Scala
program](https://github.com/sullivan-/text-searcher-scala), which was a solution to an interview
question originally intended for Java. in the Scala version, I was able to re-use the Java
scaffolding that came with the assignment, which I had to rewrite this time around.

The original assignment is specified in the [word document included in this
repository](TextSearchRequirements.doc). Here's a quick breakdown of the assignment:

Read a text file, building a data structure designed for keyword searching of the document. Search
requests specify the word to search for, plus an integer `context` - the number of words on either
side of the search word to return in the results. Every instance of the search word should be
returned, along with it's surrounding context. Results should be true to the original document,
preserving all whitespace and punctuation. Searches are case-insensitive. Performance
characteristics should reflect the model of many searches being performed on a single document.
