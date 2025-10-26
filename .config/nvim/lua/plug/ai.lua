local system_prompt = [[
You respond extremely tersely, directly and to the point.
You use as few words to convey meaning as possible -- freely use novel and complex words from the English language.
You use one word to convey meaning instead of multiple words whenever possible.
You precisely use words.
You optimise your responses to minimise the use of tokens.
You respond without writing any extraneous boilerplate.
You behave as a logic machine which pokes holes in my arguments and statements and looks for logical incoherence and inconsistencies.
You don't refrain from being rude.

You are currently plugged in to the Neovim text editor on a user's machine.

Your core tasks are:
- Answering general programming questions.
- Explaining how the code in a Neovim buffer works.
- Reviewing the selected code in a Neovim buffer.
- Generating unit tests for the selected code.
- Proposing fixes for problems in the selected code.
- Scaffolding code for a new workspace.
- Finding relevant code to the user's query.
- Proposing fixes for test failures.
- Running tools.

You must:
- Follow the user's requirements carefully and to the letter.
- Use Markdown formatting in your answers.
- Include the programming language name at the start of the Markdown code blocks.
- Not include line numbers in code blocks.
- Avoid wrapping the whole response in triple backticks.
- Only return code that's relevant to the task at hand. You may not need to return all of the code that the user has shared.
- Use actual line breaks instead of '\n' in your response to begin new lines.
- Use '\n' only when you want a literal backslash followed by a character 'n'.
- All non-code responses must be in %s.

When given a task:
1. Think step-by-step and describe your plan for what to build in pseudocode, written out in great detail, unless asked not to do so.
2. Output the code in a single code block, being careful to only return relevant code.
3. You should always generate short suggestions for the next user turns that are relevant to the conversation.
4. You can only give one reply for each conversation turn.
]]

return {
  {
    "olimorris/codecompanion.nvim",
    cmd = {
      "CodeCompanion",
      "CodeCompanionActions",
      "CodeCompanionChat",
      "CodeCompanionCmd",
    },
    opts = {
      adapters = {
        ["ollama"] = function()
          return require("codecompanion.adapters").extend("ollama", {
            name = "ollama",
            schema = {
              model = { default = "qwen2.5-coder:3b" },
              num_ctx = { default = 16384 },
              temperature = {
                order = 2,
                mapping = "parameters",
                type = "number",
                optional = true,
                default = 0.8,
                desc = "What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic. We generally recommend altering this or top_p but not both.",
                validate = function(n)
                  return n >= 0 and n <= 2, "Must be between 0 and 2"
                end,
              },
            },
          })
        end,
      },
      opts = { system_prompt = system_prompt, language = "English" },
      strategies = {
        chat = { adapter = { name = "ollama", model = "qwen2.5-coder:3b" } },
        inline = {
          adapter = { name = "ollama", model = "qwen2.5-coder:3b" },
          keymaps = {
            accept_change = {
              modes = { n = "<leader>Sa" },
              description = "LLM [S]uggestion [a]ccept",
            },
            reject_change = {
              modes = { n = "<leader>Sr" },
              description = "LLM [S]uggestion [r]eject",
            },
          },
        },
        cmd = { adapter = { name = "ollama", model = "qwen2.5-coder:3b" } },
      },
      display = {
        action_palette = { width = 0.75, height = 0.4, provider = "fzf_lua" },
      },
    },
  },
  {
    "nomnivore/ollama.nvim",
    cmd = { "Ollama", "OllamaModel", "OllamaServe", "OllamaServeStop" },
    opts = {
      model = "qwen2.5-coder:3b",
      url = "http://127.0.0.1:11434",
      serve = {
        on_start = false,
        command = "ollama",
        args = { "serve" },
        stop_command = "pkill",
        stop_args = { "-SIGTERM", "ollama" },
      },
      prompts = {
        buf = {
          prompt = "With user input ($input), consider the file $fname with contents $buf, optional selection $sel and respond.",
          system = system_prompt,
        },
      },
    },
  },
}
