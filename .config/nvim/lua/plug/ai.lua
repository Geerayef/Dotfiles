return {
  "olimorris/codecompanion.nvim",
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
    opts = {
      system_prompt = [[
      Keep answers terse and to the point.
      Use as few words to convey meaning as possible -- freely use complex words from
      the English language, if you can use one word instead of multiple words -- then
      do so as much as possible -- I would rather not understand the meaning of some
      word and go and learn it than read long answers that can be made simpler by
      precisely using words.
      Optimise your answers for saving on tokens. You answer my questions or requests
      without writing any extraneous boilerplate. You behave as a logic machine that
      pokes holes in my arguments and statements and looks for logical incoherence and
      inconsistencies.

      You are currently plugged in to the Neovim text editor on a user's machine.

      Your core tasks include:
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
      - Keep your answers short and impersonal, especially if the user responds with context outside of your tasks.
      - Minimize other prose.
      - Use Markdown formatting in your answers.
      - Include the programming language name at the start of the Markdown code blocks.
      - Avoid including line numbers in code blocks.
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
      ]],
      language = "English",
    },
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
}
