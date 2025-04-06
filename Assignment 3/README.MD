# **README**
# Assignment 3: Sorting and Removing Duplicates in a PS File Using REXX

## Overview
This assignment involves creating a Physical Sequential (PS) file with **120 records**, each **100 bytes long**. The records contain a **primary key located between columns 25 and 32**.  
Among the 120 records:  
- **15 are duplicates**  
- **105 are unique**  
- **Records are stored in random order**  

The objective is to write a **REXX program** to:  
✅ **Remove duplicate records**  
✅ **Sort the remaining records based on the primary key (columns 25-32)**  

---

## 📂 Folder Structure
```
Assignment3/
│-- processRecords.rexx      # REXX script to remove duplicates and sort records
│-- input.txt                # Input PS file with 120 records (including duplicates)
│-- output.txt               # Output file with sorted and unique records
```

## 📝 Steps to Complete the Assignment  

### 🔹 Step 1: Create the Input File (PSINPUT)  
1. **Open ISPF (Interactive System Productivity Facility).**  
2. Navigate to **Option 3.2 (Data Set Utility)** to create a new PS file.  
3. Enter the dataset name **`userid.DATA.PSINPUT`** and specify:  
   - **RECFM:** FB (Fixed Block)  
   - **LRECL:** 100 (Record Length of 100 bytes)  
   - **BLKSIZE:** 1000 (Optional)  
4. Save the dataset.  
5. **Edit `userid.DATA.PSINPUT` using ISPF 3.4** and manually enter **120 records**:  
   - Ensure the **primary key is located in columns 25-32**.  
   - Include **15 duplicate records** and keep the records in **random order**.  

---

### 🔹 Step 2: Write the REXX Script (REMOVE_SORT.rexx)  
1. Inside the **`Assignment3/`** folder, create a new **REXX script** named **`REMOVE_SORT.rexx`**.  
2. Open the file and write a **REXX program** to:  
   - Read records from **`userid.DATA.PSINPUT`**.  
   - Identify and **remove duplicate records**.  
   - **Sort** the remaining **unique records** based on the primary key (columns **25-32**).  
   - Write the processed records to **`userid.DATA.PSOUTPUT`**.  

---

### 🔹 Step 3: Execute the REXX Script  
1. Open the **TSO Command Prompt**.  
2. Navigate to the directory containing **`REMOVE_SORT.rexx`**.  
3. Run the REXX script using:  
   ```rexx
   EXEC 'userid.REXX.REMOVE_SORT'
