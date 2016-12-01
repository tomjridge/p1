naive_xs:=p0.x p0_template.x 

core_xs:=p1_prelude.x p1_core.x

lib_xs:=p1_lib.x

test_xs:=p0_test.x p1_examples.x

# for lib
xs:=$(naive_xs) $(core_xs) $(lib_xs)

$(lib_xs): $(core_xs)
