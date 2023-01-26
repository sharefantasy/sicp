local _2afile_2a = ".lnvim.fnl"
local _2amodule_name_2a = "scip-local"
local _2amodule_2a
do
  package.loaded[_2amodule_name_2a] = {}
  _2amodule_2a = package.loaded[_2amodule_name_2a]
end
local _2amodule_locals_2a
do
  _2amodule_2a["aniseed/locals"] = {}
  _2amodule_locals_2a = (_2amodule_2a)["aniseed/locals"]
end
local nvim = require("nvim-local-fennel.aniseed.nvim")
do end (_2amodule_locals_2a)["nvim"] = nvim
nvim.g["conjure#client#guile#socket#pipename"] = ".guile-repl.socket"
return _2amodule_2a