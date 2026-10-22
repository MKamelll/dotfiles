vim.o.number = true
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.expandtab = true
vim.o.autoindent = true
vim.o.smartindent = true
vim.o.clipboard = "unnamedplus"
vim.o.hlsearch = false
vim.o.swapfile = false
vim.g.mapleader = " "

-- diagnostics
vim.keymap.set("n", "fl", function()
    vim.diagnostic.setqflist({ open = true })
end)

-- use a to enter insert mode because i keep hitting fucking u or p ffs
vim.keymap.set("n", "a", "i")
vim.keymap.set("n", "i", "<nop>")

vim.keymap.set("n", "A", "o")
vim.keymap.set("n", "o", "<nop>")

-- jump a whole paragraph
vim.keymap.set({ "n", "v" }, "<C-Down>", "}")
vim.keymap.set({ "n", "v" }, "<C-Up>", "{")
vim.keymap.set("i", "<C-Down>", "<C-o>}")
vim.keymap.set("i", "<C-Up>", "<C-o>{")

-- move lines
vim.keymap.set("n", "<M-Up>", ":m .-2<CR>==")
vim.keymap.set("n", "<M-Down>", ":m .+1<CR>==")
vim.keymap.set("v", "<M-Up>", ":m '<-2<CR>gv=gv")
vim.keymap.set("v", "<M-Down>", ":m '>+1<CR>gv=gv")

-- Tab indents like Emacs electric-indent
vim.keymap.set("n", "<Tab>", ">>")
vim.keymap.set("n", "<S-Tab>", "<<")
vim.keymap.set("v", "<Tab>", ">gv")
vim.keymap.set("v", "<S-Tab>", "<gv")

--[[ -- delete trailing whitespace on save -- ]]
vim.api.nvim_create_autocmd("BufWritePre", {
    callback = function()
        vim.cmd([[%s/\s\+$//e]])
    end,
})

vim.filetype.add({
    extension = {
        djhtml = "htmldjango",
    },
})


-- general bindings
vim.keymap.set("n", "<leader>w", ":w <CR>", { desc = "Write file" })
vim.keymap.set("n", "<leader>q", ":q <CR>", { desc = "Quit" })
vim.keymap.set('n', '<C-Right>', 'e', { desc = 'Move forward by word' })
vim.keymap.set('n', '<C-Left>', 'ge', { desc = 'Move backward by word' })
vim.keymap.set('i', '<C-Right>', '<C-o>e', { desc = 'Move forward by word' })
vim.keymap.set('i', '<C-Left>', '<C-o>ge', { desc = 'Move backward by word' })

-- buffers
vim.keymap.set("n", "<leader>bn", ":bnext<CR>", { desc = "Buffer next" })
vim.keymap.set("n", "<leader>bp", ":bprevious<CR>", { desc = "Buffer previous" })
vim.keymap.set("n", "<leader>bd", ":bdelete<CR>", { desc = "Buffer delete" })
vim.keymap.set("n", "<leader>bm", ":b #<CR>", { desc = "Switch to most recent buffer" })

-- tabs
vim.keymap.set("n", "<leader>tn", ":tabnext<CR>", { desc = "Tab next" })
vim.keymap.set("n", "<leader>tp", ":tabprevious<CR>", { desc = "Tab previous" })
vim.keymap.set("n", "<leader>td", ":tabclose<CR>", { desc = "Tab close" })


-- lsp
vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, { desc = "goto definition" })
