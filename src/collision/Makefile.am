noinst_LIBRARIES = libwindstille_collision.a

libwindstille_collision_a_SOURCES = \
  collision_mask.hxx collision_mask.cxx \
  collision_sprite.hxx collision_sprite.cxx

bin_PROGRAMS = colltest collision_benchmark collision_sprite_test

colltest_SOURCES = colltest.cxx

collision_benchmark_SOURCES = collision_benchmark.cxx
collision_benchmark_LDADD = libwindstille_collision.a

collision_sprite_test_SOURCES = collision_sprite_test.cxx
collision_sprite_test_LDADD = libwindstille_collision.a

# EOF #