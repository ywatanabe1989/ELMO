param(
    [Parameter(Mandatory=$true)]
    [string]$targetPath,
    [Parameter(Mandatory=$true)]
    [string]$linkPath
)

Write-Host "Debug: PowerShell script started"
Write-Host "Debug: Target path: $targetPath" 
Write-Host "Debug: Link path: $linkPath"

$targetPath = $targetPath -replace "\\", "\\"
$linkPath = $linkPath -replace "\\", "\\"

if (Test-Path $linkPath) {
    Write-Host "Debug: Removing existing link"
    Remove-Item $linkPath -Force -Confirm:$false -Recurse
}

Write-Host "Debug: Creating new symlink"
New-Item -ItemType SymbolicLink -Path $linkPath -Target $targetPath -Force -Confirm:$false
