# Create necessary directories if they don't exist
$dirs = @(
    "data\raw",
    "data\processed",
    "output\figures\wind_turbine",
    "output\figures\shopping",
    "docs\wind_turbine",
    "docs\shopping",
    "src\wind_turbine_analysis",
    "src\shopping_analysis"
)

foreach ($dir in $dirs) {
    if (-not (Test-Path $dir)) {
        New-Item -ItemType Directory -Path $dir -Force
    }
}

# Move BD_final_02 files
Get-ChildItem "BD_final_02" -Filter "*.png" | ForEach-Object {
    Move-Item $_.FullName "output\figures\shopping\" -Force
}

Get-ChildItem "BD_final_02" -Filter "*.pdf" | ForEach-Object {
    Move-Item $_.FullName "docs\shopping\" -Force
}

Get-ChildItem "BD_final_02" -Filter "*.perf" | ForEach-Object {
    Move-Item $_.FullName "data\raw\" -Force
}

if (Test-Path "BD_final_02\code.R") {
    Move-Item "BD_final_02\code.R" "src\shopping_analysis\analysis.R" -Force
}

# Move BD_final_01 files
Get-ChildItem "BD_final_01" -Filter "*.png" | ForEach-Object {
    Move-Item $_.FullName "output\figures\wind_turbine\" -Force
}

Get-ChildItem "BD_final_01" -Filter "*.pdf" | ForEach-Object {
    Move-Item $_.FullName "docs\wind_turbine\" -Force
}

Get-ChildItem "BD_final_01" -Filter "*.docx" | ForEach-Object {
    Move-Item $_.FullName "docs\wind_turbine\" -Force
}

if (Test-Path "BD_final_01\data") {
    Move-Item "BD_final_01\data" "data\raw\wind_turbine_data" -Force
}

# Clean up empty directories
Remove-Item "BD_final_01" -Recurse -Force
Remove-Item "BD_final_02" -Recurse -Force
