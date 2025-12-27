# PMR Optimization Changelog

## Version 1.1 - 2025-12-09 (CRITICAL FIX)

### 🔧 Bug Fix: Separate Single Pack Configs for Cable vs IPTV

**Issue Identified:**
The original optimized version incorrectly used the same single pack config file for both Cable and IPTV processing. In reality:
- **Cable TV** uses its own single pack config
- **IPTV** uses 4 separate single pack configs (one for each week: 7th, 14th, 21st, 28th)

**What Was Fixed:**

1. **Renamed Cable Single Pack Config Variable**
   - Changed `plan_config` → `cable_plan_config` for clarity
   - Only used in Cable PMR processing

2. **Separated Service-Channel Map Creation**
   - Cable processing creates `service_channel_map` from Cable plan config
   - IPTV processing either:
     - Reuses Cable's `service_channel_map` if Cable was processed first
     - Creates its own from IPTV plan config if running IPTV-only

3. **Updated File Selection Prompts**
   - Cable: "Select Single Pack Config file for DPO processing"
   - IPTV: "Select IPTV Single Pack Config - 7th day" (etc.)
   - Added clear note: "These are IPTV-specific single pack configs (different from Cable)"

4. **Added Fallback Logic**
   - When running IPTV-only mode, script now prompts for package details and plan config separately

### File Selection Flow (Updated)

#### **When Running Both Cable + IPTV:**

```
1. Google Sheets loaded automatically (plan_names, bouquet_names, trai_names)

=== CABLE PROCESSING ===
2. Select MQ Bouquet data file
3. Select MQ Alacarte data file
4. Select Package Details file
5. Select Plan Config file (with services)
6. Select Single Pack Config for CABLE DPO processing  ← CABLE SPECIFIC

=== IPTV PROCESSING ===
7. Select Main IPTV subscription file (Excel)
8. Select IPTV Single Pack Config - 7th day   ← IPTV SPECIFIC
9. Select IPTV Single Pack Config - 14th day  ← IPTV SPECIFIC
10. Select IPTV Single Pack Config - 21st day ← IPTV SPECIFIC
11. Select IPTV Single Pack Config - 28th day ← IPTV SPECIFIC

Total: 11 file selections
```

#### **When Running IPTV-Only:**

```
1. Google Sheets loaded automatically

=== IPTV PROCESSING ===
2. Select Package Details file
3. Select Plan Config file (30-day billing)
4. Select Main IPTV subscription file (Excel)
5. Select IPTV Single Pack Config - 7th day
6. Select IPTV Single Pack Config - 14th day
7. Select IPTV Single Pack Config - 21st day
8. Select IPTV Single Pack Config - 28th day

Total: 8 file selections
```

#### **When Running Cable-Only:**

```
1. Google Sheets loaded automatically

=== CABLE PROCESSING ===
2. Select MQ Bouquet data file
3. Select MQ Alacarte data file
4. Select Package Details file
5. Select Plan Config file (with services)
6. Select Single Pack Config for CABLE DPO processing

Total: 6 file selections
```

---

## Code Changes Summary

### Modified Functions

#### `main_generate_pmr()` - Lines 641-706

**Before:**
```r
if (process_cable) {
  service_channel_map <- process_plan_config(...)
  plan_config <- read.csv(file.choose())  # ← Used for both Cable and IPTV
  ...
}

if (process_iptv) {
  generate_iptv_pmr(..., service_channel_map, ...)  # ← Problem: undefined if cable=FALSE
}
```

**After:**
```r
service_channel_map <- NULL  # Initialize

if (process_cable) {
  service_channel_map <- process_plan_config(...)
  cable_plan_config <- read.csv(file.choose())  # ← Cable-specific
  ...
}

if (process_iptv) {
  # Create service_channel_map if Cable wasn't processed
  if (is.null(service_channel_map)) {
    pack_details <- read.csv(file.choose())
    plan_config_iptv <- read.csv(file.choose())
    service_channel_map <- process_plan_config(plan_config_iptv, pack_details)
  }

  iptv_data <- load_iptv_data_interactive()  # ← Loads IPTV-specific single packs
  generate_iptv_pmr(..., service_channel_map, ...)
}
```

#### `load_mq_data_interactive()` - Lines 123-145

**Added clear prompts:**
```r
message("\n=== LOAD CABLE MQ DATA FILES ===")
message("Select MQ Bouquet data file...")
message("Select MQ Alacarte data file...")
# etc.
```

#### `load_iptv_data_interactive()` - Lines 356-385

**Added warning note:**
```r
message("\n=== LOAD IPTV SINGLE PACK CONFIGS (4 weeks) ===")
message("NOTE: These are IPTV-specific single pack configs (different from Cable)")
```

---

## Testing Recommendations

### Test Case 1: Both Cable + IPTV (Default)
```r
results <- main_generate_pmr()
```

**Expected:**
- Prompts for 11 files total
- Cable single pack config used only for Cable processing
- IPTV single pack configs (4 files) used only for IPTV processing
- All outputs generated correctly

### Test Case 2: Cable Only
```r
results <- main_generate_pmr(process_cable = TRUE, process_iptv = FALSE)
```

**Expected:**
- Prompts for 6 files total
- No IPTV processing
- Only Cable outputs generated

### Test Case 3: IPTV Only
```r
results <- main_generate_pmr(process_cable = FALSE, process_iptv = TRUE)
```

**Expected:**
- Prompts for 8 files total
- Creates service_channel_map from IPTV plan config
- Only IPTV outputs generated
- **THIS IS THE CRITICAL TEST** - this would have failed in v1.0

---

## Migration from v1.0 to v1.1

### If You're Already Using v1.0:

**Option A: Download Fresh Copy**
1. Save your existing `PMR_REPORT_OPTIMIZED.R` as `PMR_REPORT_OPTIMIZED_v1.0.R`
2. Download new v1.1 version
3. Test thoroughly

**Option B: Manual Patch**
Replace the `main_generate_pmr()` function (lines 641-706) with the updated version.

### Breaking Changes

None - the fix is backward compatible. The script will just ask for the correct files now.

---

## Understanding the Single Pack Configs

### Cable Single Pack Config
- **Purpose:** Maps DPO package codes to bouquets/alacarte for Cable TV
- **Format:** Contains columns like `Plan.Name`, `X` (Bouquet/Alacarte), `Bouquet`
- **Used in:** `generate_cable_pmr()` → `process_dpo_bouquets()`, `process_dpo_alacarte()`
- **File Count:** 1 file

### IPTV Single Pack Configs (4 files)
- **Purpose:** Maps IPTV plan codes to bouquets/alacarte for each week of the month
- **Format:** Contains columns like `Code`, `Bouquet`, `X`, `Broadcaster.Name`
- **Used in:** `merge_iptv_packages()` where each week's data is merged separately
- **File Count:** 4 files (7th, 14th, 21st, 28th day snapshots)
- **Why 4 files?** IPTV package configurations may change during the month, so each week needs its own config

---

## Visual Comparison

### Original Script (PMR_REPORT_MAKE_NEW.R)

```
Cable Processing:
  ↓
  Load single pack config (line 46, 122)
  ↓
  Use for DPO processing

[73 lines of code]

Berhampore Cable Processing:  ← DUPLICATE
  ↓
  Load SAME single pack config (line 122)
  ↓
  Use for DPO processing

[73 lines of code]

IPTV Processing:
  ↓
  Load 4 single pack configs (lines 173-176)
  ↓
  Use for IPTV merge
```

### v1.0 (INCORRECT - Single Pack Config Issue)

```
Cable Processing:
  ↓
  Load single pack config
  ↓
  Use for ALL areas (Network + Berhampore)
  ↓
  service_channel_map created

IPTV Processing:
  ↓
  Load 4 single pack configs
  ↓
  Use Cable's service_channel_map  ← PROBLEM if cable=FALSE
```

### v1.1 (CORRECT - This Version)

```
Cable Processing:
  ↓
  Load CABLE single pack config
  ↓
  Use for ALL areas (Network + Berhampore)
  ↓
  service_channel_map created from CABLE data

IPTV Processing:
  ↓
  IF service_channel_map exists:
    Use it (from Cable)
  ELSE:
    Create new service_channel_map from IPTV plan config
  ↓
  Load 4 IPTV single pack configs
  ↓
  Use IPTV single packs for merge
```

---

## Key Learnings

1. **Cable vs IPTV have different data structures**
   - Don't assume they use the same configuration files

2. **Scope management matters**
   - Variables created in one `if` block may not exist in another
   - Always initialize shared variables or add fallback logic

3. **Clear naming prevents confusion**
   - `plan_config` → ambiguous
   - `cable_plan_config` vs IPTV's `singlepack_7` → clear

4. **User prompts should guide the user**
   - Generic "Select file..." → confusing
   - "Select CABLE Single Pack Config..." → clear

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| **1.1** | 2025-12-09 | Fixed Cable/IPTV single pack config separation |
| **1.0** | 2025-12-09 | Initial optimized version (had single pack bug) |

---

## Support

If you encounter issues with v1.1:

1. Check that you're selecting the correct files:
   - Cable single pack: 1 file for DPO processing
   - IPTV single packs: 4 files (one per week)

2. Verify file formats match expected columns

3. Test each mode independently:
   - Cable only
   - IPTV only
   - Both together

4. Compare outputs with original `PMR_REPORT_MAKE_NEW.R` to validate

---

**End of Changelog**
