local capabilities = require("cmp_nvim_lsp").default_capabilities()

vim.lsp.config("emmet_ls", {
	capabilities = capabilities,
	cmd = { "emmet-ls", "--stdio" },
	filetypes = {
		"astro",
		"css",
		"eruby",
		"html",
		"htmlangular",
		"htmldjango",
		"javascriptreact",
		"less",
		"pug",
		"sass",
		"scss",
		"svelte",
		"templ",
		"typescriptreact",
		"vue",
	},
	root_markers = { ".git" },
})
vim.lsp.enable("emmet_ls")
