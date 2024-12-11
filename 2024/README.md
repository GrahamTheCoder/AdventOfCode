Much of this folder is AI generated code. I'm practicing my prompting and LLM dev workflow on these problems.

Sometimes the LLM will just solve the whole thing, but when it doesn't the idea is to get as much of the way along this journey as possible.
1. A clear problem definition
2. A separate parsing function (which is usually easy, but boring to code)
3. The rough shape of a solution
4. The crux of the solution
5. Readable and maintainable code for a solution
6. (Optional) May do a unit test based prompt too, though an agent is better suited really.

I may highlight non-trivial prompts in the commit messages.

### Current system prompt

Temp: 0.73, Top P: 0.64, Model: 4o

I'll provide a wordy problem definition. Please think through all the bits that could be relevant and any relationships between parts, then give a brief summary of the information actually needed to solve the problem.
The problem usually uses an esoteric input format, write a parse function in python which can read it from a file into a corresponding data structure relevant to the problem using best practices in python. Then write a separate function which operates on that data model to solve the problem. Give all the code as one executable python file that reads input from inputs/00.txt.
