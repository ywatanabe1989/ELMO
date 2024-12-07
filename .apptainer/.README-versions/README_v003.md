<!-- ---
!-- title: ./self-evolving-agent/apptainer/README.md
!-- author: ywatanabe
!-- date: 2024-12-08 00:24:04
!-- --- -->


## Build SEA

``` python
# Build
apptainer build --fakeroot --sandbox ./apptainer/sea.sandbox ./apptainer/sea.def

# Run SEA server
apptainer run --fakeroot ./apptainer/sea.sandbox
```

