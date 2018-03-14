(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is uFrmOption.pas
 *
 * The Initial Developer of the Original Code is
 * Shinji Chikugi
 *
 * Portions created by the Initial Developer are Copyright (C) 2010-
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit uFrmOption;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, uFormatterOption, ExtCtrls, ImgList, IOUtils;

type  
  TfrmOption = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    Label3: TLabel;
    edtCommandLine: TEdit;
    chkUseUTF8: TCheckBox;
    edtExePath: TButtonedEdit;
    edtConfigPath: TButtonedEdit;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    ImageList1: TImageList;
    procedure edtExePathRightButtonClick(Sender: TObject);
    procedure edtConfigPathRightButtonClick(Sender: TObject);
  private
    { Private êÈåæ }
  public
    { Public êÈåæ }
    procedure SetOption(Option: TFormatterOption);
    function GetOption(out Option: TFormatterOption): Boolean;
  end;
var
  frmOption: TfrmOption;

implementation

{$R *.dfm}
procedure TfrmOption.SetOption(Option: TFormatterOption);
begin
  with Option do begin
    edtExePath.Text := FExePath;
    edtConfigPath.Text := FConfigPath;
    edtCommandLine.Text := FCommandLine;
    chkUseUTF8.Checked := FUseUTF8;
  end;
end;


procedure TfrmOption.edtExePathRightButtonClick(Sender: TObject);
begin
  with OpenDialog1 do begin
    FileName := edtConfigPath.Text;
    InitialDir := TPath.GetDirectoryName(FileName);

    if Execute()  = true then begin
        edtExePath.Text := FileName;
    end;
  end;

end;

procedure TfrmOption.edtConfigPathRightButtonClick(Sender: TObject);
begin
  with OpenDialog2 do begin
    FileName := edtConfigPath.Text;
    InitialDir := TPath.GetDirectoryName(FileName);

    if Execute()  = true then begin
        edtConfigPath.Text := FileName;
    end;
  end;
end;


function TfrmOption.GetOption(out Option: TFormatterOption): Boolean;
begin
  with Option do begin
    FExePath := edtExePath.Text;
    FConfigPath := edtConfigPath.Text;
    FCommandLine := edtCommandLine.Text;
    FUseUTF8 := chkUseUTF8.Checked;
  end;
  Result := True;
end;


end.
