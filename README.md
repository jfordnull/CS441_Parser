# CS441_Parser
 A parser in racket for CS441

Unfortunately, this parser is not fully-functioning. I didn't have time to get functions or while loops to parse
correctly. I stubbornly chose not to rely on any libraries, which after talking to a few peers I think was a mistake.
This parser works by reading in a file and separating each line of the file into a list of strings, separated by
whitespace. Those lists (lines) are appended into one superlist of lists which represent a program. Those strings are
then mapped to tokens. I thought this was a smart shortcut, but it instead created a new set of problems - some of which
I solved and some of which I didn't. I wonder if there's just a conceptual problem too with trying to map strings as
tokens before I know if they're part of higher-order constructs in the language. (Like while loops and functions)

I hope the evidence of effort and some structure here is enough to earn me a few points. All you need to do to parse
is: (parse <yourfilenameinquotes>), assuming Parser.rkt is in the same working directory as your program.

If you're curious about the representation of lines as lists of tokens, I recommend uncommenting the (displayln tokens)
function within the isLine? function.

Code is pretty thoroughly commented. A docx file with my LLM prompts and a few sources cited is also present.
