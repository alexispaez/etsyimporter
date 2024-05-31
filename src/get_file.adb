with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;

with AWS.Client;
with AWS.Default;
with AWS.Headers;
with AWS.Resources;
with AWS.Response;
with AWS.Messages;

procedure Get_File (URL : String; Out_File_Name : String) is
   use AWS;

   Data        : Response.Data;
   User        : Unbounded_String;
   Pwd         : Unbounded_String;
   Proxy       : Unbounded_String;
   Proxy_User  : Unbounded_String;
   Proxy_Pwd   : Unbounded_String;
   Client_Cert : constant Unbounded_String :=
     To_Unbounded_String (Default.Client_Certificate);
   Follow_Redirection : constant Boolean := False;
   Header             : Headers.List;
begin

   Data :=
     Client.Get
       (URL,
        To_String (User),
        To_String (Pwd),
        To_String (Proxy),
        To_String (Proxy_User), To_String (Proxy_Pwd),
        Follow_Redirection => Follow_Redirection,
        Headers => Header,
        Certificate => To_String (Client_Cert));

   Ada.Text_IO.Put_Line
     ("Status Code = "
      & Messages.Image (Response.Status_Code (Data))
      & " - "
      & Messages.Reason_Phrase (Response.Status_Code (Data)));

   declare
      use Ada.Streams;

      Message_Stream : Resources.File_Type;
      Buffer         : Stream_Element_Array (1 .. 4_096);
      Last           : Stream_Element_Offset;
   begin
      Response.Message_Body (Data, Message_Stream);

      declare
         F : Ada.Streams.Stream_IO.File_Type;
      begin
         Stream_IO.Create (F,
                           Stream_IO.Out_File,
                           Out_File_Name);

         loop
            Resources.Read (Message_Stream, Buffer, Last);

            Stream_IO.Write (F, Buffer (1 .. Last));

            exit when Last < Buffer'Last;
         end loop;

         Stream_IO.Close (F);
      end;

      Resources.Close (Message_Stream);
   end;
end Get_File;
