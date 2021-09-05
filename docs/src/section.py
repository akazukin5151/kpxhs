from pathlib import Path

STARTING_COMMENT = '-- EMBED START'
ENDING_COMMENT = '-- EMBED END'

with open('../src/kpxhs/Config/Defaults.hs', 'r') as f:
    f = f.readlines()

for idx, line in enumerate(f):
    if line == f'{STARTING_COMMENT}\n':
        break

# Skip the comment, the type signature, and the function assignment
offset = idx + 3

section = []
for line in f[offset:]:
    if line != f'{ENDING_COMMENT}\n':
        section.append(line[2:])
    else:
        break

Path('out').mkdir(exist_ok=True)
Path('out/default_theme.hs').touch(exist_ok=True)

with open('out/default_theme.hs', 'w+') as f:
    f.write(''.join(section))
