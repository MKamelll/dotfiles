vim.opt.number = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.o.clipboard = "unnamedplus"
vim.o.hlsearch = false
vim.o.swapfile = false

-- diagnostics
vim.keymap.set("n", "fl", function()
    vim.diagnostic.setqflist({ open = true })
end)

-- use a to enter insert mode because i keep hitting fucking u or p ffs
vim.keymap.set("n", "a", "i")
vim.keymap.set("n", "i", "<nop>")

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

-- netrw
vim.keymap.set("n", "ee", ":Ex <CR>", { desc = "Open Netrw" })
vim.keymap.set("n", "eee", ":Vexplore <CR>", { desc = "Open Netrw in vertical window" })

-- general bindings
vim.keymap.set("n", "ww", ":w <CR>", { desc = "Write file" })
vim.keymap.set("n", "qq", ":q <CR>", { desc = "Quit" })

-- buffers
vim.keymap.set("n", "bn", ":bnext<CR>", { desc = "Buffer next" })
vim.keymap.set("n", "bp", ":bprevious<CR>", { desc = "Buffer previous" })
vim.keymap.set("n", "bd", ":bdelete<CR>", { desc = "Buffer delete" })
vim.keymap.set("n", "bm", ":b #<CR>", { desc = "Switch to most recent buffer" })

-- tabs
vim.keymap.set("n", "tn", ":tabnext<CR>", { desc = "Tab next" })
vim.keymap.set("n", "tp", ":tabprevious<CR>", { desc = "Tab previous" })
vim.keymap.set("n", "td", ":tabclose<CR>", { desc = "Tab close" })
