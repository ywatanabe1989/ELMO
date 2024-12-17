# Helper function
ensure_sdir() {
    local spath="$1"
    mkdir -p "$(dirname "$spath")" > /dev/null
}
