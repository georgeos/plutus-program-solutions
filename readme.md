# Plutus pioneer program solutions

This repo has all my solutions related to the Plutus Pioneer program, part of the Cardano ecosystem. I was part of the Second Cohort.

### Disclaimer
Those solutions are not necessarily the correct ones, since during the course of the program, I was in a learning process. Therefore, they shouldn't be taken as a final solution or to implement in a production environment.

## Visual code

After installing and using nix-shell.

1. Create a root folder, in my case ```~/cardano``` and add all your repos to it:
- plutus: https://github.com/input-output-hk/plutus
- plutus-pioneer-program: https://github.com/input-output-hk/plutus-pioneer-program
- plutus-program-solutions: this repo (my solutions)
2. Open visual studio code.
3. Create a workspace on root folder: ```cardano.code-workspace```
4. Add all repos desired to the workspace.
5. Close visual studio code
6. Open a nix-shell and go to root folder
7. Execute ```code cardano.code-workspace```
8. That's all! Now you have lint and help docs from visual code.