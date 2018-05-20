-- Merge two tables, overwriting entires in the first one
-- in case of a conflict.
function table_merge(t1, t2)
   for k,v in pairs(t2) do
      if type(v) == "table" then
         if type(t1[k] or false) == "table" then
            table_merge(t1[k] or {}, t2[k] or {})
         else
            t1[k] = v
         end
      else
         t1[k] = v
      end
   end
   return t1
end

function table_concat(t1, t2)
    for i=1,#t2 do
        t1[#t1+1] = t2[i]
    end
    return t1
end

function table_length(t)
  local count = 0
  for _ in pairs(t) do count = count + 1 end
  return count
end
