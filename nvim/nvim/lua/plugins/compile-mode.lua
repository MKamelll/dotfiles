return {
    "ej-shafran/compile-mode.nvim",
    version = "^5.0.0",
    dependencies = {
        "nvim-lua/plenary.nvim",
        { "m00qek/baleia.nvim", tag = "v1.3.0" },
    },
    config = function()
        ---@type CompileModeOpts
        vim.g.compile_mode = {
            input_word_completion = true,
            baleia_setup = true,
            -- to make `:Compile` replace special characters (e.g. `%`) in
            -- the command (and behave more like `:!`), add:
            -- bang_expansion = true,
        }
    end
}
