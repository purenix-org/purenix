
{ unsafeHas = builtins.hasAttr;
  unsafeGet = builtins.getAttr;
  unsafeSet = strKey: val: attrSet: attrSet // { ${strKey} = val; };
  unsafeDelete = strKey: attrSet: builtins.removeAttrs attrSet [ strKey ];
}
