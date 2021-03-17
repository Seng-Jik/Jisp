
if(-not (Test-Path "./obj")) {
    md "./obj"
}

[string[]] $project = Get-Content "./project.yml"
[string[]] $src = @()
[string] $projectName = ""

foreach($projectLine in $project) {
    [string[]] $kv = $projectLine -Split ":"
    $key = $kv[0].Trim()
    $value = $kv[1].Trim()
    if($key -eq "project") { $projectName = $value }
    elseif($key -eq "type") { 
        if($value -ne "app") { 
            Write-Error "jisp-project must be a jisp app."
        }
    }
    elseif($key -eq "src") {
        $srcContent = Get-Content ./src/$value
        foreach($srcLine in $srcContent) {
            $src += $srcLine
        }
    }
    else {
        Write-Error ("Unknown property " + $key + ".")
    }
}

Set-Content -Path ./obj/$projectName.jisp -Value $src

jisp "./obj/jisp-project.jisp" run build

