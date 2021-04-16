
create proc out_object_result_err(o object not null)
begin
  declare C cursor like out_object_result_err arguments;
  fetch C from arguments;
  out C;
end;
