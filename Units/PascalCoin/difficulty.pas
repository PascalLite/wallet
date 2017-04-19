// This file is a part of PascalLite cryptocurrency project
//
// Copyright (C) 2017 PascalLite project
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// Author: xiphon <https://github.com/xiphon>, April 2017

unit difficulty;

{$MODE Delphi}

interface

uses
  UConst,
  UCrypto,
  Classes,
  SysUtils,
  math;

type
  TDifficulty = class(TObject)
  public
    class function GetNextTarget(const timestamps : array of QWord; const previousTarget : TRawBytes) : TRawBytes;
  end;

const
  DIFFICULTY_HARDFORK_HEIGHT = 29000;
  DIFFICULTY_BLOCKS = 10;
  DIFFICULTY_TIMESTAMPS = DIFFICULTY_BLOCKS + 1;

implementation

uses
  UBlockChain;

class function TDifficulty.GetNextTarget(const timestamps : array of QWord; const previousTarget : TRawBytes) : TRawBytes;
var
  median : QWord;
  multiplier1 : Int64;
  multiplier2 : Int64;
  previous : TBigNum;
  targetHash : TBigNum;
begin
  Result := previousTarget;

  if Length(timestamps) < 2 then
  begin
    exit;
  end;

  median := (timestamps[0] - timestamps[Length(timestamps) - 1]) div Min(DIFFICULTY_BLOCKS, Length(timestamps) - 1);

  multiplier1 := Max(0, 4 - (median DIV 50));
  multiplier1 := multiplier1 * multiplier1 * multiplier1;

  multiplier2 := Max(-86400, Min(0, 300 - median));

  previous := TBigNum.Create;
  try
    targetHash := TBigNum.Create;
    try
      targetHash.RawValue := previousTarget;
      previous.RawValue := previousTarget;

      previous.Divide(512);
      previous.Multiply(multiplier1);
      targetHash.Sub(previous);

      previous.RawValue := previousTarget;
      previous.Divide(86400);
      previous.Multiply(multiplier2);
      targetHash.Sub(previous);

      Result := TPCBank.TargetFromCompact(TPCBank.TargetToCompact(targetHash.RawValue));
    finally
      targetHash.Free;
    end;
  finally
    previous.Free;
  end;
end;

end.
