require('nvim-treesitter').install { "c", "lua", "vim", "vimdoc", "query",
    "markdown", "markdown_inline", "python",
    "go", "javascript", "typescript", "java", "svelte", "rust", "htmldjango", "html", "zig" }

vim.api.nvim_create_autocmd("FileType", {
    callback = function(ev)
        pcall(vim.treesitter.start, ev.buf)
    end
})
