AM_CXXFLAGS = -Wall

noinst_LIBRARIES = libwindstille_collision.a

libwindstille_collision_a_SOURCES = \
  collision_mask.hxx collision_mask.cxx

bin_PROGRAMS = colltest collision_benchmark

colltest_SOURCES = colltest.cxx

collision_benchmark_SOURCES = collision_benchmark.cxx

collision_benchmark_LDADD = libwindstille_collision.a

# EOF #