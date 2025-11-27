vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.o.clipboard = "unnamedplus"

-- move lines
vim.keymap.set("n", "<M-Up>", ":m .-2<CR>==")
vim.keymap.set("n", "<M-Down>", ":m .+1<CR>==")

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

vim.keymap.set("n", "fs", ":Ex<CR>", { desc = "Opens directory structure" })

-- navigation
vim.keymap.set("n", "bn", ":bnext<CR>", { desc = "Buffer next" })
vim.keymap.set("n", "bp", ":bprevious<CR>", { desc = "Buffer previous" })
vim.keymap.set("n", "bd", ":bdelete<CR>", { desc = "Buffer delete" })

vim.keymap.set("n", "tn", ":tabnext<CR>", { desc = "Tab next" })
vim.keymap.set("n", "tp", ":tabprevious<CR>", { desc = "Tab previous" })
vim.keymap.set("n", "td", ":tabclose<CR>", { desc = "Tab close" })
