local mc = require("multicursor-nvim")
mc.setup()

local set = vim.keymap.set

-- Add or skip cursor above/below the main cursor.
set({ "n", "x" }, "<c-d>", function() mc.lineAddCursor(1) end)
set({ "n", "x" }, "<c-s-d>", function() mc.lineSkipCursor(1) end)

-- Add or skip adding a new cursor by matching word/selection
set({ "v", "x" }, "<c-d>", function() mc.matchAddCursor(1) end)
set({ "v", "x" }, "<c-s-d>", function() mc.matchSkipCursor(1) end)

mc.addKeymapLayer(function(layerSet)
    layerSet("n", "<esc>", function()
        if not mc.cursorsEnabled() then
            mc.enableCursors()
        else
            mc.clearCursors()
        end
    end)
end)

-- Customize how cursors look.
local hl = vim.api.nvim_set_hl
hl(0, "MultiCursorCursor", { reverse = true })
hl(0, "MultiCursorVisual", { link = "Visual" })
hl(0, "MultiCursorSign", { link = "SignColumn" })
hl(0, "MultiCursorMatchPreview", { link = "Search" })
hl(0, "MultiCursorDisabledCursor", { reverse = true })
hl(0, "MultiCursorDisabledVisual", { link = "Visual" })
hl(0, "MultiCursorDisabledSign", { link = "SignColumn" })
