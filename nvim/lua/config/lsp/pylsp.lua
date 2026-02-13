local capabilities = require("cmp_nvim_lsp").default_capabilities()
vim.lsp.config("pylsp", {
    capabilities = capabilities,
    cmd = { "pylsp" },
    filetypes = { "python" },
    root_markers = {
        "pyproject.toml",
        "setup.py",
        "setup.cfg",
        "requirements.txt",
        "Pipfile",
        ".git",
    },
})
vim.lsp.enable("pylsp")
