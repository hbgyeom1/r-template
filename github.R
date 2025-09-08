usethis::use_git_config(user.name = "hbgyeom1", user.email = "hbgyeom@gmail.com")

# credentials::credential_helper_set("manager")    # Windows
credentials::credential_helper_set("osxkeychain")  # Mac
# credentials::credential_helper_set("store")      # Linux

usethis::create_github_token() 

credentials::set_github_pat()

usethis::git_sitrep()
