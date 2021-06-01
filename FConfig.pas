unit FConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, fpjson;

type
  TFormConfig = class(TForm)
    BtOK: TButton;
    BtCancel: TButton;
    EdTime: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure BtOKClick(Sender: TObject);
    procedure BtCancelClick(Sender: TObject);
  private

  public

  end;

var
  FormConfig: TFormConfig;

implementation

{$R *.lfm}

{ TFormConfig }

procedure TFormConfig.BtOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TFormConfig.BtCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

