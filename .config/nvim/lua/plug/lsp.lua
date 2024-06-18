return {
  "williamboman/mason.nvim",
  cmd = "Mason",
  opts = {
    registries = { "github:mason-org/mason-registry" },
    ui = {
      icons = {
        package_installed = S.Icons.ui.box_check,
        package_pending = S.Icons.ui.arrow_r,
        package_uninstalled = S.Icons.ui.box_empty,
      },
      border = S.Border,
      width = 0.7,
      height = 0.5,
    },
  },
}
