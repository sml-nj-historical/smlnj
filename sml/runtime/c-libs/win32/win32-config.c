/* win32-config.c
 *
 * COPYRIGHT (c) 2008, the Fellowship of SML/NJ
 *
 * interface to win32 system configuration information
 */

#include <windows.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

ml_val_t _ml_win32_CONFIG_get_version_ex(ml_state_t *msp, ml_val_t arg)
{
  OSVERSIONINFOEX versionInfo;
  long result = 0;
  int length = 0;
  ml_val_t res, major, minor, build, platform, csd, vec;

  ZeroMemory(&versionInfo, sizeof(OSVERSIONINFOEX));
  versionInfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFOEX);

  result = GetVersionEx((OSVERSIONINFO *)&versionInfo);
  if (result == 0) {
    return RAISE_SYSERR(msp,-1);
  }

  WORD_ALLOC(msp, major, (Word_t)versionInfo.dwMajorVersion);
  WORD_ALLOC(msp, minor, (Word_t)versionInfo.dwMinorVersion);
  WORD_ALLOC(msp, build, (Word_t)versionInfo.dwBuildNumber);
  WORD_ALLOC(msp, platform, (Word_t)versionInfo.dwPlatformId);

  length = strlen(versionInfo.szCSDVersion);
  vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS (length + 1));
  strcpy_s(PTR_MLtoC(void, vec), length+1, versionInfo.szCSDVersion);
  SEQHDR_ALLOC (msp, csd, DESC_string, vec, length);
                       
  REC_ALLOC5(msp, res, major, minor, build, platform, csd);

  return res;
}

ml_val_t _ml_win32_CONFIG_get_windows_directory(ml_state_t *msp, ml_val_t arg)
{
  TCHAR directory[MAX_PATH+1];
  DWORD dwSize = MAX_PATH+1;
  ml_val_t res, vec;

  if ((dwSize = GetWindowsDirectory(directory, dwSize)) == 0) {
    return RAISE_SYSERR(msp,-1);
  }

  vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS (dwSize+1));
  strcpy_s(PTR_MLtoC(void, vec), dwSize+1, directory);
  SEQHDR_ALLOC (msp, res, DESC_string, vec, dwSize);

  return res;
}

ml_val_t _ml_win32_CONFIG_get_system_directory(ml_state_t *msp, ml_val_t arg)
{
  TCHAR directory[MAX_PATH+1];
  DWORD dwSize = MAX_PATH+1;
  ml_val_t res, vec;

  if ((dwSize = GetSystemDirectory(directory, dwSize)) == 0) {
    return RAISE_SYSERR(msp,-1);
  }

  vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS (dwSize+1));
  strcpy_s(PTR_MLtoC(void, vec), dwSize+1, directory);
  SEQHDR_ALLOC (msp, res, DESC_string, vec, dwSize);

  return res;
}

ml_val_t _ml_win32_CONFIG_get_computer_name(ml_state_t *msp, ml_val_t arg)
{
  TCHAR name[MAX_PATH+1];
  DWORD dwSize = MAX_PATH+1;
  ml_val_t res, vec;

  if (!GetComputerName(name, &dwSize)) {
    return RAISE_SYSERR(msp,-1);
  }

  vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS (dwSize+1));
  strcpy_s(PTR_MLtoC(void, vec), dwSize+1, name);
  SEQHDR_ALLOC (msp, res, DESC_string, vec, dwSize);

  return res;
}

ml_val_t _ml_win32_CONFIG_get_user_name(ml_state_t *msp, ml_val_t arg)
{
  TCHAR name[MAX_PATH+1];
  DWORD dwSize = MAX_PATH+1;
  ml_val_t res, vec;

  if (!GetUserName(name, &dwSize)) {
    return RAISE_SYSERR(msp,-1);
  }

  vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS (dwSize));
  strcpy_s(PTR_MLtoC(void, vec), dwSize, name);
  SEQHDR_ALLOC (msp, res, DESC_string, vec, dwSize-1);

  return res;
}
