#include <windows.h>
#include <tlhelp32.h>
#include <stdio.h>

BOOL
GetProcessModule (DWORD dwPID, DWORD dwModuleID, 
		  LPMODULEENTRY32 lpMe32, DWORD cbMe32) 
{ 
    BOOL          bRet        = FALSE; 
    BOOL          bFound      = FALSE; 
    HANDLE        hModuleSnap = NULL; 
    MODULEENTRY32 me32        = {0}; 
 
    // Take a snapshot of all modules in the specified process. 

    hModuleSnap = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, dwPID); 
    if (hModuleSnap == INVALID_HANDLE_VALUE) 
        return (FALSE); 
 
    // Fill the size of the structure before using it. 

    me32.dwSize = sizeof(MODULEENTRY32); 
 
    // Walk the module list of the process, and find the module of 
    // interest. Then copy the information to the buffer pointed 
    // to by lpMe32 so that it can be returned to the caller. 

    if (Module32First(hModuleSnap, &me32)) 
    { 
        do 
        { 
            if (me32.th32ModuleID == dwModuleID) 
            { 
                CopyMemory (lpMe32, &me32, cbMe32); 
                bFound = TRUE; 
            } 
        } 
        while (!bFound && Module32Next(hModuleSnap, &me32)); 
 
        bRet = bFound;   // if this sets bRet to FALSE, dwModuleID 
                         // no longer exists in specified process 
    } 
    else 
        bRet = FALSE;           // could not walk module list 
 
    // Do not forget to clean up the snapshot object. 

    CloseHandle (hModuleSnap); 
 
    return (bRet); 
}

BOOL
GetProcessList () 
{ 
    HANDLE         hProcessSnap = NULL; 
    BOOL           bRet      = FALSE; 
    PROCESSENTRY32 pe32      = {0}; 
 
    //  Take a snapshot of all processes in the system. 

    hProcessSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); 

    if (hProcessSnap == INVALID_HANDLE_VALUE) 
        return (FALSE); 
 
    //  Fill in the size of the structure before using it. 

    pe32.dwSize = sizeof(PROCESSENTRY32); 
 
    //  Walk the snapshot of the processes, and for each process, 
    //  display information. 

    if (Process32First(hProcessSnap, &pe32)) 
    { 
        DWORD         dwPriorityClass; 
        BOOL          bGotModule = FALSE; 
        MODULEENTRY32 me32       = {0}; 
 
        do 
        { 
            bGotModule = GetProcessModule(pe32.th32ProcessID, 
                pe32.th32ModuleID, &me32, sizeof(MODULEENTRY32)); 

            if (bGotModule) 
            { 
                HANDLE hProcess; 
 
                // Get the actual priority class. 
                hProcess = OpenProcess (PROCESS_ALL_ACCESS, 
                    FALSE, pe32.th32ProcessID); 
                dwPriorityClass = GetPriorityClass (hProcess); 
                CloseHandle (hProcess); 

                // Print the process's information. 
                printf( "\nPriority Class Base\t%d\n", 
                    pe32.pcPriClassBase); 
                printf( "PID\t\t\t%d\n", pe32.th32ProcessID);
                printf( "Thread Count\t\t%d\n", pe32.cntThreads);
                printf( "Module Name\t\t%s\n", me32.szModule);
                printf( "Full Path\t\t%s\n\n", me32.szExePath);
            } 
        } 
        while (Process32Next(hProcessSnap, &pe32)); 
        bRet = TRUE; 
    } 
    else 
        bRet = FALSE;    // could not walk the list of processes 
 
    // Do not forget to clean up the snapshot object. 

    CloseHandle (hProcessSnap); 
    return (bRet); 
}

int 
main ()
{
  GetProcessList ();
  return 0;
}
