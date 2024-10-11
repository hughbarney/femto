--local logging = require 'logging'
local function index(list,needle)
   for i,element in ipairs(list) do
      if element == needle then return i end
   end
end
return {
   {
      Inlines = function(list)
	 local l = pandoc.List {}
	 for i,elem in ipairs(list) do
	    if elem.tag == 'Code' then
	       local last = l[#l]
	       if last and last.tag == 'Code' and not index(last.attr.classes, 'variable')  then
		  l:remove()
		  local text = elem.text
		  if index(elem.attr.classes, 'variable') then
		     text = '«'..text..'»'
		  end
		  combined = pandoc.Code(last.text..text, last.attr)
		  l:insert(combined)
	       else
		  if index(elem.attr.classes, 'variable') then
		     l:insert(pandoc.Emph(elem.text))
		  else
		     l:insert(elem)
		  end
	       end
	    else
	       l:insert(elem)
	    end
	 end
	 return l
      end
   }
}
