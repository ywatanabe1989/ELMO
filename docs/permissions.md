<!-- ---
!-- title: ./Semacs/docs/permissions.md
!-- author: ywatanabe
!-- date: 2024-12-08 03:33:20
!-- --- -->

755 breakdown:
- 7 (owner/host): full permissions (read=4, write=2, execute=1)
- 5 (semacs): read & execute only (read=4, execute=1)
- 5 (others): read & execute only

This is a good security practice where:
1. Host user maintains full control (7)
2. Semacs user can read/execute but not write (5)
3. Others have same restricted access (5)
