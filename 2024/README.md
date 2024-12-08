Add any common setup here that happens per folder

```powershell
python -m venv .venv
.\.venv\Scripts\Activate.ps1
New-Item -ItemType File -Path "requirements.txt"
```