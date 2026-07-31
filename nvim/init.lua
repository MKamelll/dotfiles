vim.pack.add({
    "https://github.com/numtostr/comment.nvim",
    "https://github.com/stevearc/conform.nvim",
    "https://github.com/blazkowolf/gruber-darker.nvim",
    "https://github.com/lukas-reineke/indent-blankline.nvim",
    "https://github.com/miikanissi/modus-themes.nvim",
    "https://github.com/jake-stewart/multicursor.nvim",
    "https://github.com/neovim/nvim-lspconfig",
    "https://github.com/rachartier/tiny-inline-diagnostic.nvim",
})

vim.pack.add({
  "https://github.com/nvim-lua/plenary.nvim",
  { src = "https://github.com/m00qek/baleia.nvim", version = "v1.3.0" },
  { src = "https://github.com/ej-shafran/compile-mode.nvim", version = "v5.0.0" },
})

vim.g.compile_mode = {
    input_word_completion = true,
    baleia_setup = true,
}

vim.pack.add({
  "https://github.com/hrsh7th/cmp-nvim-lsp",
  "https://github.com/hrsh7th/cmp-buffer",
  "https://github.com/hrsh7th/cmp-path",
  "https://github.com/L3MON4D3/LuaSnip",
  "https://github.com/saadparwaiz1/cmp_luasnip",
  "https://github.com/rafamadriz/friendly-snippets",
  "https://github.com/hrsh7th/nvim-cmp",
})

vim.pack.add({
  "https://github.com/nvim-lua/plenary.nvim",
  { src = "https://github.com/nvim-telescope/telescope.nvim", version = "v0.1.9" },
})

vim.pack.add({
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = "master" },
})

vim.api.nvim_create_autocmd("PackChanged", {
  callback = function(ev)
    if ev.data.spec.name == "nvim-treesitter" and ev.data.kind ~= "delete" then
      vim.cmd("TSUpdate")
    end
  end,
})

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

require("config")
