#!/bin/bash

# Script to update copyright information from CIWTA to Interworldly Adventuring, LLC
# This script updates all copyright notices to reflect the ownership transfer

echo "Updating copyright information from CIWTA to Interworldly Adventuring, LLC..."

# Update main README.org
sed -i 's/Corporation for Inter-World Tourism and Adventuring (interworldly.com)/Interworldly Adventuring, LLC of Portland, OR, USA/g' README.org
sed -i 's/Interworldly Adventuring, LLC/Interworldly Adventuring, LLC/g' README.org

# Update all Lisp files in src/ directory
find src/ -name "*.lisp" -type f -exec sed -i 's/Corporation for Inter-World Tourism and Adventuring (interworldly.com)/Interworldly Adventuring, LLC of Portland, OR, USA/g' {} \;

# Update all documentation files
find doc/ -type f -exec sed -i 's/Corporation for Inter-World Tourism and Adventuring (interworldly.com)/Interworldly Adventuring, LLC of Portland, OR, USA/g' {} \;

# Update texi-to-html file
sed -i 's/Corporation for Inter-World Tourism and Adventuring (interworldly.com)/Interworldly Adventuring, LLC of Portland, OR, USA/g' texi-to-html

# Update copyright year ranges to include 2024-2025
find . -name "*.lisp" -o -name "*.org" -o -name "*.texi" | xargs sed -i 's/Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021/Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021, 2024-2025/g'
find . -name "*.lisp" -o -name "*.org" -o -name "*.texi" | xargs sed -i 's/Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021 The/Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2021, 2024-2025 The/g'

# Update specific CIWTA references
find . -type f -exec sed -i 's/Interworldly Adventuring, LLC/Interworldly Adventuring, LLC/g' {} \;
find . -type f -exec sed -i 's/interworldly.com/interworldly.com/g' {} \;

echo "Copyright update completed for server repository."
