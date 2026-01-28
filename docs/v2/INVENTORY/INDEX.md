# Module Inventory System - Complete Index

Quick navigation for erlmcp v2 module analysis and cleanup.

## Start Here

1. **[README.md](README.md)** - Overview and usage guide
   - What is the inventory system?
   - How to use the files
   - Statistics and insights
   - V2 architectural recommendations

## Deep Dives

### Complete Module Catalog
- **[module_inventory.md](module_inventory.md)** (641 lines)
  - All 244 active modules listed by family
  - 29 module families analyzed
  - Exports documented for each module
  - File sizes and statistics
  - Complete index of all code

### Cleanup Planning
- **[duplicate_families.md](duplicate_families.md)** (202 lines)
  - 10 module families with duplicates identified
  - 6 broken files marked for deletion
  - 6 backup files marked for deletion
  - Clear keep vs delete recommendations
  - Automated cleanup shell script included
  - 243 KB technical debt to reclaim

### Technical Reference
- **[GENERATION_REPORT.txt](GENERATION_REPORT.txt)** (301 lines)
  - Complete technical documentation
  - Extraction methodology explained
  - Classification rules documented
  - Reliability notes and limitations
  - V2 kickoff checklist

## Machine-Readable Format

- **[inventory.json](../../inventory.json)** (root level, 269 KB)
  - Machine-readable database of all modules
  - Query with jq or parse programmatically
  - Complete metadata: exports, sizes, families
  - Suitable for build tool integration

## Regeneration Tool

- **[tools/v2_arch/extract_module_inventory.py](../../tools/v2_arch/extract_module_inventory.py)**
  - Run anytime to regenerate inventory
  - Usage: `python3 tools/v2_arch/extract_module_inventory.py`
  - Updates inventory.json automatically

## Quick Stats

| Metric | Value |
|--------|-------|
| Active modules | 244 |
| Module families | 29 |
| Broken files | 6 |
| Duplicate files | 6 |
| Total codebase size | ~4.3 MB |
| Cleanup candidates | 12 files (243 KB) |

## Recommended Reading Order

**For Planning:**
1. README.md (10 min) - Get the overview
2. GENERATION_REPORT.txt (15 min) - Understand the details
3. duplicate_families.md (5 min) - Get specific cleanup plan

**For Implementation:**
1. duplicate_families.md (execute cleanup script)
2. module_inventory.md (understand full structure)
3. GENERATION_REPORT.txt (reference section on V2 actions)

**For Development:**
1. README.md (understand families and structure)
2. module_inventory.md (find your module)
3. GENERATION_REPORT.txt (understand classification rules)

## V2 Architectural Decisions

Based on this inventory analysis, key v2 recommendations:

1. **Delete 12 files** (243 KB technical debt)
   - 6 broken modules (.broken files)
   - 6 backup variants (.backup, _new, etc.)

2. **Rationalize utilities family** (28 → 7 modules)
   - Currently too broad
   - Should split into specific families

3. **Consolidate configuration** (7 → 4 modules)
   - loader, profiles, schema, validation overlap

4. **Review monitoring** (13 modules across 4 families)
   - Consolidate metrics/monitoring/health overlap

5. **Maintain transport structure** (10 modules)
   - Clean abstraction, good separation of concerns

## File Format Details

### inventory.json Structure
```json
{
  "timestamp": 1769570089,
  "src_directory": "src",
  "summary": {
    "total_modules": 244,
    "num_families": 29,
    "num_broken": 6,
    "num_duplicates": 6
  },
  "families": {
    "family_name": [
      {
        "file": "src/module.erl",
        "module": "module_name",
        "family": "family",
        "exports": ["func/1", ...],
        "num_exports": N,
        "size_bytes": N
      }
    ]
  },
  "broken": [...],
  "duplicates": [...]
}
```

### Markdown Files Format
- Headers with statistics
- Tables for quick lookup
- Code examples and scripts
- Organized by family

## How to Use Each File

### module_inventory.md
```bash
# Find all modules in a family
grep "^### family_name" module_inventory.md -A 20

# Count modules per family
grep "^### " module_inventory.md | wc -l

# Search for specific module
grep "erlmcp_client" module_inventory.md
```

### duplicate_families.md
```bash
# Get cleanup recommendations
grep "Recommendation:" duplicate_families.md

# Find which files to delete
grep "DELETE" duplicate_families.md

# Extract cleanup script
sed -n '/^```bash/,/^```$/p' duplicate_families.md > cleanup.sh
chmod +x cleanup.sh
./cleanup.sh
```

### inventory.json
```bash
# Query with jq
jq '.families | keys' inventory.json          # List all families
jq '.summary' inventory.json                   # Get statistics
jq '.broken[].file' inventory.json             # Get broken files
jq '.families.transport' inventory.json        # Get transport modules
jq '.all_modules | length' inventory.json      # Count total modules
```

## Next Steps

1. [ ] Read README.md to understand the system
2. [ ] Review duplicate_families.md for cleanup plan
3. [ ] Execute cleanup script when ready
4. [ ] Plan v2 family reorganization
5. [ ] Update this index as v2 evolves

## Support

- Questions about extraction? See GENERATION_REPORT.txt "Technical Details"
- Need to regenerate? Use tools/v2_arch/extract_module_inventory.py
- Want to extend? The Python script is well-commented and modular

---

**Generated:** 2026-01-27
**Tool:** extract_module_inventory.py
**Documentation:** 1,321 lines across 4 files
**Next refresh:** When src/ directory changes significantly
