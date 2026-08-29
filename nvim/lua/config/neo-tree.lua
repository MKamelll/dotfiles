require("neo-tree").setup({
    filesystem = {
        filtered_items = {
            visible = true, -- Shows hidden files (e.g., .gitignore) but keeps them faded
            hide_dotfiles = false,
            hide_gitignored = false,
        },
        follow_current_file = {
            enabled = true,            -- Focuses the tree on the file you currently have open
        },
        use_libuv_file_watcher = true, -- Automatically updates the tree if files change externally
    },
    window = {
        position = "right",
        width = 30,
        mappings = {
            ["l"] = "open",       -- Use 'l' to open a folder or file
            ["h"] = "close_node", -- Use 'h' to collapse a folder
        },
    },
})

-- Auto-quit Neovim if Neo-tree is the last window remaining
vim.api.nvim_create_autocmd("BufEnter", {
    group = vim.api.nvim_create_augroup("NeoTreeAutoClose", { clear = true }),
    callback = function()
        local stats = vim.uv.fs_stat(vim.api.nvim_buf_get_name(0))
        if vim.fn.winnr("$") == 1 and vim.bo.filetype == "neo-tree" then
            vim.cmd("quit")
        end
    end,
})

vim.keymap.set("n", "ee", ":Neotree toggle<CR>", { desc = "toggle neotree" })
