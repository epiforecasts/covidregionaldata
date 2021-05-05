if (nrow(switchrManifest() %>% filter(name == "newcovidregionaldata"))==1) {
  removeLib("newcovidregionaldata")
}
if (nrow(switchrManifest() %>% filter(name == "oldcovidregionaldata"))==1) {
  removeLib("oldcovidregionaldata")
}

crd_new <- cranPkgVersManifest(pkg = "covidregionaldata", vers = "0.9.1")
crd_old <- cranPkgVersManifest(pkg = "covidregionaldata", vers = "0.9.0")

switchTo("oldcovidregionaldata", seed = crd_old)
switchBack()
switchTo("newcovidregionaldata", seed = crd_new)
switchBack()
