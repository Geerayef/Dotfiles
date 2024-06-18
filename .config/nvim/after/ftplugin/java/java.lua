local home = os.getenv("HOME")
local jdtls = require("jdtls")
local jdtls_setup = require("jdtls.setup")
local root_markers = { "build.gradle", "gradlew", "mvnw", ".git", "*.java" }
local root_dir = require("jdtls.setup").find_root(root_markers)
local workspace_folder = home
  .. "/.cache/jdtls/workspace"
  .. vim.fn.fnamemodify(root_dir, ":p:h:t")
local mason_packages = home .. "/.local/share/nvim/mason/packages"
local path_to_jdtls = mason_packages .. "/jdtls"
local path_to_config = path_to_jdtls .. "/config_linux"
local lombok_path = path_to_jdtls .. "/lombok.jar"
local path_to_jar = path_to_jdtls
  .. "/plugins/org.eclipse.equinox.launcher_1.6.600.v20231106-1826.jar"
local cmp = require("cmp_nvim_lsp")
local capabilities = {
  workspace = { configuration = true },
  textDocument = { completion = { completionItem = { snippetSupport = true } } },
}
capabilities = vim.tbl_deep_extend(
  "force",
  {},
  vim.lsp.protocol.make_client_capabilities(),
  cmp.default_capabilities(capabilities) or {},
  capabilities or {}
)

-- ~  LSP Attach

local function lsp_attach(client, bufnr)
  jdtls_setup.add_commands()
  Key.LSP(client, bufnr)
  Key.JDTLS()
  F.LspAttach(client, bufnr)
  require("lsp_signature").on_attach({
    bind = true,
    padding = "",
    handler_opts = { border = "rounded" },
    hint_prefix = "󱄑 ",
  }, bufnr)
end

-- ~  Config

local extendedClientCapabilities = require("jdtls").extendedClientCapabilities
extendedClientCapabilities.resolveAdditionalTextEditsSupport = true

local config = {
  flags = { debounce_text_changes = 80, allow_incremental_sync = true },
  on_attach = lsp_attach,
  capabilities = capabilities,
  root_dir = root_dir,
  init_options = { extendedClientCapabilities = extendedClientCapabilities },
  settings = {
    java = {
      format = {
        enabled = true,
        settings = {
          url = "/.local/share/eclipse/eclipse-java-google-style.xml",
          profile = "GoogleStyle",
        },
      },
      eclipse = { downloadSources = true },
      maven = { downloadSources = true },
      signatureHelp = { enabled = true },
      contentProvider = { preferred = "fernflower" },
      completion = {
        favoriteStaticMembers = {
          "org.hamcrest.MatcherAssert.assertThat",
          "org.hamcrest.Matchers.*",
          "org.hamcrest.CoreMatchers.*",
          "org.junit.jupiter.api.Assertions.*",
          "java.util.Objects.requireNonNull",
          "java.util.Objects.requireNonNullElse",
          "org.mockito.Mockito.*",
        },
        filteredTypes = {
          "com.sun.*",
          "io.micrometer.shaded.*",
          "java.awt.*",
          "jdk.*",
          "sun.*",
        },
      },
      sources = {
        organizeImports = { starThreshold = 9999, staticStarThreshold = 9999 },
      },
      codeGeneration = {
        toString = {
          template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}",
        },
        hashCodeEquals = { useJava7Objects = false },
        useBlocks = true,
      },
      -- And search for `interface RuntimeOption`
      -- configuration = { runtimes = { { name = "Java-21", path =  "/path/to/jvm" } } }
    },
  },
  cmd = {
    "java",
    "-Declipse.application=org.eclipse.jdt.ls.core.id1",
    "-Dosgi.bundles.defaultStartLevel=4",
    "-Declipse.product=org.eclipse.jdt.ls.core.product",
    "-Dlog.protocol=true",
    "-Dlog.level=ALL",
    "-Xmx4g",
    "--add-modules=ALL-SYSTEM",
    "--add-opens",
    "java.base/java.util=ALL-UNNAMED",
    "--add-opens",
    "java.base/java.lang=ALL-UNNAMED",
    "-javaagent:" .. lombok_path,
    -- vim.fn.glob("/usr/share/java/jdtls/plugins/org.eclipse.equinox.launcher_*.jar"),
    "-jar",
    path_to_jar,
    -- "/usr/share/java/jdtls/config_linux",
    "-configuration",
    path_to_config,
    "-data",
    workspace_folder,
  },
}

jdtls.start_or_attach(config)
