local capabilities = require("cmp_nvim_lsp").default_capabilities()

vim.lsp.config("pylsp", {
    capabilities = capabilities
})
vim.lsp.enable("pylsp")

vim.lsp.config("emmet_ls", {
    capabilities = capabilities
})
vim.lsp.enable("emmet_ls")

vim.lsp.config("djls", {
    capabilities = capabilities
})
vim.lsp.enable("djls")

vim.lsp.config("ts_ls", {
    capabilities = capabilities
})
vim.lsp.enable("ts_ls")

vim.lsp.config("lua_ls", {
    capabilities = capabilities
})
vim.lsp.enable("lua_ls")
