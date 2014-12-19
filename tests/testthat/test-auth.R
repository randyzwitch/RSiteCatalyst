expect_output(SCAuth(Sys.getenv("USER", ""), Sys.getenv("SECRET", "")),
              "Authentication Succeeded.")
