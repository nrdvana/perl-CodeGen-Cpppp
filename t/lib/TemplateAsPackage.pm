package TemplateAsPackage;
use CodeGen::Cpppp::Template -setup => 0;

template_builder->parse(<<'CPPPP', __FILE__, __LINE__+1);
## param $p0 = 1;
## param $p1 = 2;

Initial Line of Output

## sub more_lines($text, $count) {
$text ## for 1..$count;
## }
CPPPP

1;
