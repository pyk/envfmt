set dotenv-load

repomix subcommand:
    #!/usr/bin/env bash
    if [[ {{subcommand}} == "envfmt" ]] then
        npx repomix@latest . --style xml \
            -o repomix-envfmt-$(date +%Y%m%d-%H%M%S).xml
    fi
