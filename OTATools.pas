unit OTATools;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolsAPI, Menus, TypInfo;

function GetIDEMainMenu: TMainMenu;
function GetCurrentSourceEditor: IOTASourceEditor;
function GetTopMostEditView: IOTAEditView;
function BooleanText(const Value: Boolean): string;
procedure WriteTitleMessage(const Title: string);
procedure WriteToolMessage(const FileName, MessageStr, PrefixStr: string; LineNumber, ColumnNumber: Integer);
procedure ClearToolMessages;
procedure GxOtaShowEditViewDetails;
procedure GxOtaAssertSourceEditorNotReadOnly(SourceEditor: IOTASourceEditor);
procedure GxOtaReplaceEditorText(SourceEditor: IOTASourceEditor; Text: string);
procedure GxOtaReplaceSelection(const Editor: IOTASourceEditor; ViewNum: Integer; const Text: string);
procedure GxOtaSaveReaderToStream(EditReader: IOTAEditReader; Stream: TStream; TrailingNull: Boolean);
procedure GxOtaLoadSourceEditorToUnicodeStrings(SourceEditor: IOTASourceEditor; Data: TStrings);
function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean): string;
function GxOtaGetActiveEditorText(Lines: TStrings; UseSelection: Boolean): Boolean;
function IsCharLineEndingOrNull(C: AnsiChar): Boolean;
function IsCharLineEnding(C: Char): Boolean;
function EOLSizeAtPos(const S: string; Pos: Integer): Integer;
procedure RemoveLastEOL(var S: string);

const
  EditReaderBufferSize = 1024 * 24;

implementation

function GetIDEMainMenu: TMainMenu;
begin
  Result := (BorlandIDEServices as INTAServices).MainMenu;
end;

function GetCurrentSourceEditor: IOTASourceEditor;
var
  i: Integer;
  Editor: IOTAEditor;
  ISourceEditor: IOTASourceEditor;
  CurrentModule: IOTAModule;
begin
  Result := nil;
  CurrentModule := (BorlandIDEServices as IOTAModuleServices).CurrentModule;

  for i := 0 to CurrentModule.GetModuleFileCount - 1 do
  begin
    Editor := CurrentModule.GetModuleFileEditor(i);

    if Supports(Editor, IOTASourceEditor, ISourceEditor) then
    begin
      Result := ISourceEditor;
      Break;
    end;
  end;
end;

function GetTopMostEditView: IOTAEditView;
var
  ModuleServices: IOTAModuleServices;
begin
  Result := nil;
  ModuleServices := (BorlandIDEServices as IOTAModuleServices);
  if ModuleServices.ModuleCount > 0 then
  begin
    Result := (BorlandIDEServices as IOTAEditorServices).TopView;
  end;
end;

function BooleanText(const Value: Boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

procedure WriteTitleMessage(const Title: string);
var
  IMessageServices: IOTAMessageServices;
begin
  IMessageServices := BorlandIDEServices as IOTAMessageServices;
  Assert(Assigned(IMessageServices));

  IMessageServices.AddTitleMessage(Title);
end;

procedure WriteToolMessage(const FileName, MessageStr, PrefixStr: string;
  LineNumber, ColumnNumber: Integer);
var
  IMessageServices: IOTAMessageServices;
begin
  IMessageServices := BorlandIDEServices as IOTAMessageServices;
  Assert(Assigned(IMessageServices));

  IMessageServices.AddToolMessage(FileName, MessageStr, PrefixStr, LineNumber,
    ColumnNumber);
end;

procedure ClearToolMessages;
var
  IMessageServices: IOTAMessageServices;
begin
  IMessageServices := BorlandIDEServices as IOTAMessageServices;
  if Assigned(IMessageServices) then
    IMessageServices.ClearToolMessages;
end;

procedure GxOtaShowEditViewDetails;
var
  View: IOTAEditView;
  Buffer: IOTAEditBuffer;
  // Msg: TStringList;
  Options: IOTABufferOptions;
  Window: INTAEditWindow;
  Form: TCustomForm;
  Block: IOTAEditBlock;
  Position: IOTAEditPosition;
  IMessageServices: IOTAMessageServices;
  MessageGroup: IOTAMessageGroup;

  procedure Add(const Str: string);
  begin
    IMessageServices.AddTitleMessage(Str, MessageGroup);
  end;

  procedure AddFmt(const Str: string; const Args: array of const );
  begin
    IMessageServices.AddTitleMessage(Format(Str, Args), MessageGroup);
    // Msg.Add(Format(Str, Args));
  end;

begin
  IMessageServices := BorlandIDEServices as IOTAMessageServices;
  MessageGroup := IMessageServices.AddMessageGroup('コードエディタ');

  View := GetTopMostEditView;
  if not Assigned(View) then
    raise Exception.Create('No active edit view');

  ClearToolMessages;
  // Msg := TStringList.Create;
  try
    Buffer := View.Buffer;
    if Assigned(Buffer) then
    begin
      AddFmt('FileName: %s', [Buffer.FileName]);
      AddFmt('Initial Date: %s', [DateTimeToStr(Buffer.GetInitialDate)]);
      AddFmt('Current Date: %s', [DateTimeToStr(Buffer.GetCurrentDate)]);

      Position := Buffer.EditPosition;
      if Assigned(Position) then
      begin
        AddFmt('Position.Character: %s', [Position.Character]);
        AddFmt('Position.Row: %d', [Position.Row]);
        AddFmt('Position.Column: %d', [Position.Column]);
        AddFmt('Position.IsSpecialCharacter: %s',
          [BooleanText(Position.IsSpecialCharacter)]);
        AddFmt('Position.IsWhiteSpace: %s',
          [BooleanText(Position.IsWhiteSpace)]);
        AddFmt('Position.IsWordCharacter: %s',
          [BooleanText(Position.IsWordCharacter)]);
        AddFmt('Position.LastRow: %d', [Position.LastRow]);
      end
      else
        Add('No IOTAEditView.Position');

      AddFmt('Buffer.IsModified: %s', [BooleanText(Buffer.IsModified)]);
      AddFmt('Buffer.IsReadOnly: %s', [BooleanText(Buffer.IsReadOnly)]);
      AddFmt('Buffer.BlockVisible: %s', [BooleanText(Buffer.BlockVisible)]);
      AddFmt('Buffer.Modified: %s', [BooleanText(Buffer.Modified)]);
      AddFmt('Buffer.GetLinesInBuffer: %d', [Buffer.GetLinesInBuffer]);
      AddFmt('Buffer.EditViewCount: %d', [Buffer.EditViewCount]);
      AddFmt('Buffer.BlockStart.CharIndex: %d', [Buffer.BlockStart.CharIndex]);
      AddFmt('Buffer.BlockStart.Line: %d', [Buffer.BlockStart.Line]);
      AddFmt('Buffer.BlockAfter.CharIndex: %d', [Buffer.BlockAfter.CharIndex]);
      AddFmt('Buffer.BlockAfter.Line: %d', [Buffer.BlockAfter.Line]);
      Options := Buffer.BufferOptions;
      if Assigned(Options) then
      begin
        AddFmt('AutoIndent: %s', [BooleanText(Options.AutoIndent)]);
        AddFmt('BackspaceUnindents: %s',
          [BooleanText(Options.BackspaceUnindents)]);
        AddFmt('CreateBackupFile: %s', [BooleanText(Options.CreateBackupFile)]);
        AddFmt('CursorThroughTabs: %s',
          [BooleanText(Options.CursorThroughTabs)]);
        AddFmt('GroupUndo: %s', [BooleanText(Options.GroupUndo)]);
        AddFmt('InsertMode: %s', [BooleanText(Options.InsertMode)]);
        AddFmt('KeepTrailingBlanks: %s',
          [BooleanText(Options.KeepTrailingBlanks)]);
        AddFmt('LeftGutterWidth: %d', [Options.LeftGutterWidth]);
        AddFmt('OverwriteBlocks: %s', [BooleanText(Options.OverwriteBlocks)]);
        AddFmt('PersistentBlocks: %s', [BooleanText(Options.PersistentBlocks)]);
        AddFmt('PreserveLineEnds: %s', [BooleanText(Options.PreserveLineEnds)]);
        AddFmt('RightMargin: %d', [Options.RightMargin]);
        AddFmt('SmartTab: %s', [BooleanText(Options.SmartTab)]);
        AddFmt('SyntaxHighlight: %s', [BooleanText(Options.SyntaxHighlight)]);
        AddFmt('TabStops: %s', [Options.TabStops]);
        AddFmt('UndoAfterSave: %s', [BooleanText(Options.UndoAfterSave)]);
        AddFmt('UndoLimit: %d', [Options.UndoLimit]);
        AddFmt('UseTabCharacter: %s', [BooleanText(Options.UseTabCharacter)]);
      end
      else
        Add('No buffer options');
    end
    else
      Add('No edit buffer filename');

    Block := View.Block;
    if Assigned(Block) then
    begin
      AddFmt('Block.IsValid: %s', [BooleanText(Block.IsValid)]);
      AddFmt('Block.Visible: %s', [BooleanText(Block.Visible)]);
      AddFmt('Block.EndingColumn: %d', [Block.EndingColumn]);
      AddFmt('Block.EndingRow: %d', [Block.EndingRow]);
      AddFmt('Block.StartingColumn: %d', [Block.StartingColumn]);
      AddFmt('Block.StartingRow: %d', [Block.StartingRow]);
      AddFmt('Block.Style: %s', [GetEnumName(TypeInfo(TOTABlockType),
        Ord(Block.Style))]);
      // AddFmt('Block.Text: %s', [HackBadIDEUTF8StringToString(Block.Text)]);
      AddFmt('Block.Text: %s', [Block.Text]);
    end
    else
      Add('No IOTAEditView.Block');

    AddFmt('View.BottomRow: %d', [View.BottomRow]);
    AddFmt('View.LastEditColumn: %d', [View.LastEditColumn]);
    AddFmt('View.LastEditRow: %d', [View.LastEditRow]);
    AddFmt('View.LeftColumn: %d', [View.LeftColumn]);
    AddFmt('View.RightColumn: %d', [View.RightColumn]);
    AddFmt('View.TopRow: %d', [View.TopRow]);
    AddFmt('View.CursorPos.Line: %d', [View.CursorPos.Line]);
    AddFmt('View.CursorPos.Col: %d', [View.CursorPos.Col]);
    AddFmt('View.TopPos.Line: %d', [View.TopPos.Line]);
    AddFmt('View.TopPos.Col: %d', [View.TopPos.Col]);
    AddFmt('View.ViewSize.cx: %d', [View.ViewSize.cx]);
    AddFmt('View.ViewSize.cy: %d', [View.ViewSize.cy]);

    if Supports(View, INTAEditWindow, Window) then
    begin
      Add('Supports INTAEditWindow');
      Form := Window.Form;
      if Assigned(Form) then
      begin
        AddFmt('Form Class: %s', [Form.ClassName]);
        AddFmt('Form Caption: %s', [Form.Caption]);
        AddFmt('Form Top: %d  Left: %d  Height: %d  Width: %d',
          [Form.Top, Form.Left, Form.Height, Form.Width]);
        if Assigned(Form.ActiveControl) then
          AddFmt('Form.ActiveControl ClassName: %s  TextLen: %d',
            [Form.ActiveControl.ClassName, Form.ActiveControl.GetTextLen]);
      end
      else
        Add('No INTAEditWindow.Form');
    end
    else
      Add('No support for INTAEditWindow');
  finally
    // MessageBox(Application.Handle, PChar(Msg.Text), 'Edit View Details', 0);
  end;
end;

procedure GxOtaAssertSourceEditorNotReadOnly(SourceEditor: IOTASourceEditor);
begin
  Assert(Assigned(SourceEditor));
  if Supports(SourceEditor, IOTAEditBuffer) then
    if (SourceEditor as IOTAEditBuffer).IsReadOnly then
      raise Exception.CreateFmt('%s is read only',
        [ExtractFileName(SourceEditor.FileName)]);
end;

procedure GxOtaReplaceEditorText(SourceEditor: IOTASourceEditor; Text: string);
var
  Writer: IOTAEditWriter;
begin
  Assert(Assigned(SourceEditor));
  Writer := SourceEditor.CreateUndoableWriter;
  if not Assigned(Writer) then
    raise Exception.Create('No edit writer');
  Writer.DeleteTo(MaxLongint);
  Writer.Insert(PAnsiChar(AnsiToUtf8(Text)));
  Writer := nil;
end;

procedure GxOtaReplaceSelection(const Editor: IOTASourceEditor;
  ViewNum: Integer; const Text: string);
resourcestring
  SBlockNotCompatible =
    'Unsupported block selection type for a replace operation';
var
  View: IOTAEditView;
  Start, After: TOTACharPos;

  procedure ReplaceColumns(Start, After: TOTAEditPos; RplText: string);
  begin
    raise Exception.Create
      ('Replacing of columnar selections is no longer supported');
  end;

// Replace the text between Start and After, inclusive, with Text.
// IsInclusive determines whether the last selected character is part
// of the replaced block or not.
  procedure ReplaceInclusiveExclusive(const IsInclusive: Boolean);
  var
    Writer: IOTAEditWriter;
    StartPos: Integer;
    AfterPos: Integer;
    DeleteToPos: Integer;

  begin
    if not IsInclusive then
      if After.CharIndex > 0 then
        Dec(After.CharIndex);

    StartPos := View.CharPosToPos(Start);
    Assert(StartPos >= 0, 'StartPos < 0');
    AfterPos := View.CharPosToPos(After);
    Assert(AfterPos >= 0, 'AfterPos < 0');
    Writer := Editor.CreateUndoableWriter;
    try
      // Copy the initial part of the file, up to the selection
      Writer.CopyTo(StartPos);
      // Delete the block if there is one to delete
      DeleteToPos := AfterPos;

      if (After.CharIndex = 0) and (After.Line - Start.Line = 1) then
      begin
        Dec(DeleteToPos, Length(#13#10));
      end
      else
      begin
        if After.CharIndex > 0 then
          Inc(DeleteToPos);
      end;
      if DeleteToPos > StartPos then
        Writer.DeleteTo(DeleteToPos);
      // Insert the replacement text
      Writer.Insert(PAnsiChar(AnsiToUtf8(Text)));
      // Copy the rest of the file
      Writer.CopyTo( High(Longint));
    finally
      Writer := nil;
    end;
  end;

begin
  Assert(Assigned(Editor));
  Assert(Editor.EditViewCount > ViewNum,
    'Editor.EditViewCount <= passed in ViewNum');
  GxOtaAssertSourceEditorNotReadOnly(Editor);

  View := Editor.GetEditView(ViewNum);
  Assert(Assigned(View), 'No edit view available');
  Assert(Assigned(View.Block), 'No block available');

  Start := Editor.BlockStart;
  After := Editor.BlockAfter;

  case Editor.BlockType of
    btInclusive:
      ReplaceInclusiveExclusive(True);
    btNonInclusive:
      ReplaceInclusiveExclusive(False);
    btColumn:
      ReplaceColumns(TOTAEditPos(Start), TOTAEditPos(After), Text);
    btLine:
      begin
        Start.CharIndex := 0; // Start of line
        After.CharIndex := 1023; // Max line length
        ReplaceInclusiveExclusive(True);
      end;
  else
    raise Exception.Create(SBlockNotCompatible);
  end;
end;

procedure GxOtaSaveReaderToStream(EditReader: IOTAEditReader; Stream: TStream;
  TrailingNull: Boolean);
const
  // Leave typed constant as is - needed for streaming code.
  TerminatingNullChar: AnsiChar = #0;
var
  EditReaderPos: Integer;
  ReadDataSize: Integer;
  Buffer: array [0 .. EditReaderBufferSize] of AnsiChar;
  // Array of bytes, might be UTF-8
begin
  Assert(EditReader <> nil);
  Assert(Stream <> nil);

  EditReaderPos := 0;
  ReadDataSize := EditReader.GetText(EditReaderPos, Buffer,
    EditReaderBufferSize);
  Inc(EditReaderPos, ReadDataSize);
  while ReadDataSize = EditReaderBufferSize do
  begin
    Stream.Write(Buffer, ReadDataSize);
    ReadDataSize := EditReader.GetText(EditReaderPos, Buffer,
      EditReaderBufferSize);
    Inc(EditReaderPos, ReadDataSize);
  end;
  Stream.Write(Buffer, ReadDataSize);
  if TrailingNull then
    Stream.Write(TerminatingNullChar, SizeOf(TerminatingNullChar));
  // The source parsers need this
end;

procedure GxOtaLoadSourceEditorToUnicodeStrings(SourceEditor: IOTASourceEditor;
  Data: TStrings);
var
  MemStream: TMemoryStream;
begin
  Data.Clear;
  if not Assigned(SourceEditor) then
    raise Exception.Create
      ('No source editor in GxOtaLoadSourceEditorToUnicodeStrings');
  // TODO: Check stream format for forms as text (Ansi with escaped unicode, or UTF-8) in Delphi 2007/2009
  MemStream := TMemoryStream.Create;
  try
    GxOtaSaveReaderToStream(SourceEditor.CreateReader, MemStream, False);
    MemStream.Position := 0;
{$IFDEF UNICODE}
    Data.LoadFromStream(MemStream, TEncoding.UTF8);
{$ELSE}
    if RunningDelphi8OrGreater then
      SynUnicode.LoadFromStream(Data, MemStream, seUTF8)
    else
      SynUnicode.LoadFromStream(Data, MemStream, seAnsi);
{$ENDIF}
  finally
    FreeAndNil(MemStream);
  end;
end;

function GxOtaGetCurrentSelection(IncludeTrailingCRLF: Boolean): string;
var
  EditView: IOTAEditView;
  EditBlock: IOTAEditBlock;
begin
  Result := '';

  EditView := GetTopMostEditView;
  if not Assigned(EditView) then
    Exit;

  EditBlock := EditView.Block;
  if Assigned(EditBlock) then
  begin
    // Result := IDEEditorStringToString(EditBlock.Text);
    // Result := HackBadIDEUTF8StringToString(Result);
    Result := EditBlock.Text;

    if not IncludeTrailingCRLF and (EditBlock.Style in [btNonInclusive]) then
      RemoveLastEOL(Result);
  end;
end;

function GxOtaGetActiveEditorText(Lines: TStrings;
  UseSelection: Boolean): Boolean;
var
  ISourceEditor: IOTASourceEditor;
  IEditView: IOTAEditView;
begin
  Assert(Assigned(Lines));
  Lines.Clear;
  Result := False;

  ISourceEditor := GetCurrentSourceEditor;
  if ISourceEditor = nil then
    Exit;

  if ISourceEditor.EditViewCount > 0 then
  begin
    // IEditView := GetTopMostEditView(ISourceEditor);
    IEditView := GetTopMostEditView;

    if UseSelection and Assigned(IEditView) and Assigned(IEditView.Block) and
      (IEditView.Block.Size > 0) then
      Lines.Text := GxOtaGetCurrentSelection(False)
    else
      GxOtaLoadSourceEditorToUnicodeStrings(ISourceEditor, Lines);
    Result := True;
  end;
end;

function IsCharLineEndingOrNull(C: AnsiChar): Boolean;
begin
  Result := C in [#0, #10, #13];
end;

function IsCharLineEnding(C: Char): Boolean;
begin
  Result := CharInSet(C, [#10, #13]);
end;

function StrCharAt(S: string; Pos: Integer): Char;
begin
  if (Pos >= 1) and (Pos <= Length(S)) then
    Result := S[Pos]
  else
    Result := #0;
end;

// Returns the size of any EOL characters at a given position
// Supports the following EOL formats:
// #10#13 (?)
// #13#10 (PC / Win)
// #10    (Unix)
// #13    (Macintosh, Amiga)
// Note: Pos must be at the beginning of the EOL characters
function EOLSizeAtPos(const S: string; Pos: Integer): Integer;
begin
  if IsCharLineEnding(StrCharAt(S, Pos)) then
  begin
    Result := 1;
    if (IsCharLineEnding(StrCharAt(S, Pos + 1)) and
      (StrCharAt(S, Pos) <> StrCharAt(S, Pos + 1))) then
      Inc(Result);
  end
  else
    Result := 0;
end;

procedure RemoveLastEOL(var S: string);
var
  CurrLen: Integer;
  EOLSize: Integer;
begin
  CurrLen := Length(S);
  if CurrLen > 0 then
  begin
    EOLSize := EOLSizeAtPos(S, CurrLen);
    if EOLSize > 0 then
    begin
      Dec(CurrLen);
      if EOLSizeAtPos(S, CurrLen) > EOLSize then
        // one more character found for EOL
        Dec(CurrLen);
      SetLength(S, CurrLen);
    end;
  end;
end;

end.
