local function request()
  local currentBuffer = vim.fn.bufnr()
  local escapedFilename = vim.fn.fnameescape("./client/Main.hs")
  vim.cmd("edit ".. escapedFilename)
  vim.cmd("w")
  vim.cmd("buffer " .. currentBuffer)
end

vim.keymap.set('n', '<leader>mr', request, { noremap = true, desc = "save client's main to trigger ghcid"})
