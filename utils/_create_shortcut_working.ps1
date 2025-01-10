$targetPath = "\\wsl.localhost\Ubuntu\home\ywatanabe\proj\llemacs\workspace\projects\103-epilepsty-seizure-detection"
$linkPath = "C:\Users\wyusu\Desktop\current-project-win"

# Remove existing symlink if exists
if (Test-Path $linkPath) {
    Remove-Item $linkPath -Force -Confirm:$false -Recurse
}

# Create new symlink with admin privileges
Start-Process powershell -Verb RunAs -ArgumentList "-Command New-Item -ItemType SymbolicLink -Path `"$linkPath`" -Target `"$targetPath`" -Force -Confirm:`$false"
