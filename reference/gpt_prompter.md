# Structured Prompt Builder for LLM (ChatGPT)

Build standard prompts to get the right outcomes using the four
theoretical elements required to build standard and cleaner asks:
instruction, input, context, and output. Inspired by the [Prompt
Engineering Guide](https://www.promptingguide.ai/) free guide. Remember
to start simple and be very specific to get exactly what you need.

## Usage

``` r
gpt_prompter(
  instruction = NULL,
  input = NULL,
  context = NULL,
  output = NULL,
  quiet = TRUE,
  ...
)
```

## Arguments

- instruction, input, context, output:

  Character or vector. You do not need all the four elements for a
  prompt and the format depends on the task at hand.

- quiet:

  Boolean. Should the written prompt be printed or not?

- ...:

  Additional parameters. You can pass `cols` parameter to explicitly set
  column names.

## Value

(Invisible) list with written prompt and elements provided.

## Elements of a Prompt

- Instruction:

  a specific task or instruction you want the model to perform. It
  usually starts with a verb given its an instruction.

- Input:

  input data, elements or question that we are interested to find a
  response for,

- Context:

  external information, additional context or references to steer the
  model to better responses.

- Output:

  type or format of the output.

## See also

Other ChatGPT:
[`gpt_ask()`](https://laresbernardo.github.io/lares/reference/gpt_ask.md)

Other LLM:
[`gemini_ask()`](https://laresbernardo.github.io/lares/reference/gemini_ask.md),
[`gpt_ask()`](https://laresbernardo.github.io/lares/reference/gpt_ask.md)

## Examples

``` r
# A simple formatted table with data
# Note: I mostly use output = "table" and enabled an auxiliary enrichment prompt
(p <- gpt_prompter(instruction = "Capitals of the world", output = "table"))
#> $prompt
#> [1] "Instruction = Capitals of the world ### Output = Markdown format table. If missing elements, replace with NA. For numerical columns, only use a dot for decimals, no delimiters, no abbreviations"
#> 
#> $elements
#> $elements$instruction
#> [1] "Capitals of the world"
#> 
#> $elements$input
#> NULL
#> 
#> $elements$context
#> NULL
#> 
#> $elements$output
#> [1] "Markdown format table. If missing elements, replace with NA. For numerical columns, only use a dot for decimals, no delimiters, no abbreviations"
#> 
#> 

# Classify
p <- gpt_prompter(
  instruction = "For each of the inputs, classify using only the options in context",
  input = c("Molecule", "Elephant", "Milky Way", "Cat", "Planet Earth"),
  context = c("Big", "Medium", "Small"),
  output = "table",
  # This cols parameter is auxiliary
  cols = c("Input", "Category"),
  quiet = FALSE
)
#> Instruction = For each of the inputs, classify using only the options in context
#> Input = Molecule; Elephant; Milky Way; Cat; Planet Earth
#> Context = Big; Medium; Small
#> Output = Markdown format table. If missing elements, replace with NA. For numerical columns, only use a dot for decimals, no delimiters, no abbreviations; also, it must have 2 columns named exactly: Input and Category 

# Tag all categories that apply
p <- gpt_prompter(
  instruction = paste(
    "For each of the inputs, provide which of the",
    "context values apply as correct tags using TRUE/FALSE"
  ),
  input = c("I love chocolate", "I hate chocolate", "I like Coke", "Who am I?", "T-REX"),
  context = c("food", "positive", "negative", "beverage"),
  output = "table",
  quiet = FALSE
)
#> Instruction = For each of the inputs, provide which of the context values apply as correct tags using TRUE/FALSE
#> Input = I love chocolate; I hate chocolate; I like Coke; Who am I?; T-REX
#> Context = food; positive; negative; beverage
#> Output = Markdown format table. If missing elements, replace with NA. For numerical columns, only use a dot for decimals, no delimiters, no abbreviations 

# Extract information from strings
p <- gpt_prompter(
  instruction = "For each of the inputs, extract each of the information asked in context",
  input = c("My mail is 123@test.com", "30 Main St, NY, USA", "+82 2-312-3456", "$1.5M"),
  context = c("email", "full state name", "country of phone", "full non-abbreviated number"),
  output = "table",
  cols = c("Input", "Element_to_extract", "Value"),
  quiet = FALSE
)
#> Instruction = For each of the inputs, extract each of the information asked in context
#> Input = My mail is 123@test.com; 30 Main St, NY, USA; +82 2-312-3456; $1.5M
#> Context = email; full state name; country of phone; full non-abbreviated number
#> Output = Markdown format table. If missing elements, replace with NA. For numerical columns, only use a dot for decimals, no delimiters, no abbreviations; also, it must have 3 columns named exactly: Input, Element_to_extract, and Value 

# Translate to several languages
p <- gpt_prompter(
  instruction = "For each of the inputs, translate to the respective languages in context",
  input = rep("I love you with all my heart", 5),
  context = c("spanish", "chinese", "japanese", "russian", "german"),
  output = "table",
  cols = c("Input", "Language", "Translation"),
  quiet = FALSE
)
#> Instruction = For each of the inputs, translate to the respective languages in context
#> Input = I love you with all my heart; I love you with all my heart; I love you with all my heart; I love you with all my heart; I love you with all my heart
#> Context = spanish; chinese; japanese; russian; german
#> Output = Markdown format table. If missing elements, replace with NA. For numerical columns, only use a dot for decimals, no delimiters, no abbreviations; also, it must have 3 columns named exactly: Input, Language, and Translation 

# Format date values
p <- gpt_prompter(
  instruction = paste(
    "For each of the inputs,",
    "standardize and format all values to the format in context"
  ),
  input = c("March 27th, 2021", "12-25-2023 3:45PM", "01.01.2000", "29 Feb 92"),
  context = "ISO Date getting rid of time stamps",
  output = "table",
  cols = c("Input", "Formatted"),
  quiet = FALSE
)
#> Instruction = For each of the inputs, standardize and format all values to the format in context
#> Input = March 27th, 2021; 12-25-2023 3:45PM; 01.01.2000; 29 Feb 92
#> Context = ISO Date getting rid of time stamps
#> Output = Markdown format table. If missing elements, replace with NA. For numerical columns, only use a dot for decimals, no delimiters, no abbreviations; also, it must have 2 columns named exactly: Input and Formatted 

# Convert units
p <- gpt_prompter(
  instruction = paste(
    "For each of the inputs,",
    "provide new converted values using the units in context"
  ),
  input = c("50C", "300K", "100F", "0F", "32C", "0K"),
  context = "Fahrenheit",
  output = "table",
  cols = c("Input", "Original_Unit", "Total_Value", "Converted_Value", "New_Unit"),
  quiet = FALSE
)
#> Instruction = For each of the inputs, provide new converted values using the units in context
#> Input = 50C; 300K; 100F; 0F; 32C; 0K
#> Context = Fahrenheit
#> Output = Markdown format table. If missing elements, replace with NA. For numerical columns, only use a dot for decimals, no delimiters, no abbreviations; also, it must have 5 columns named exactly: Input, Original_Unit, Total_Value, Converted_Value, and New_Unit 

# Read a text and answer a question related to it
gpt_prompter(
  instruction = "read",
  context = "Long text here",
  input = "Question here"
)$prompt
#> [1] "Instruction = You are a pro content editor who will read the context text and reply using a single concise response to the question asked in input. If the answer is not found in context, reply you cannot find the answer in the text ### Input = Question here ### Context = Long text here"
```
