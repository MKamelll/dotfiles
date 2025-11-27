vim.lsp.config("djls", {
	cmd = { "djls", "serve" },
	filetypes = { "htmldjango", "html", "python" },
	root_markers = { "manage.py", "pyproject.toml", ".git" },
})
vim.lsp.enable("djls")
