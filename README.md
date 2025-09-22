# Fortran Module: datastructs-fortran

## Description

The Fortran module `datastructs-fortran` provides a comprehensive collection of common data structures and utility functions. It includes implementations for **dynamic lists** (`dynamical_list_t`), **fixed-size lists** (`fixed_list_t`), and **max heaps** (`maxheap_t`). Additionally, it offers various **random sampler algorithms** (through `sampler_base_t` and `choose_sampler`) for weighted selection, and a **hashing function** (`djb2`).

The samplers depend on the [`rndgen-fortran`](https://github.com/wcota/rndgen-fortran/) random number generator.

## Usage

To integrate `datastructs-fortran` into your project, add it as a dependency using the Fortran Package Manager (Fpm). Include the following lines in your `fpm.toml` file:

```toml
[dependencies]
datastructs-fortran.git = "https://github.com/wcota/datastructs-fortran"
```

After adding the dependency, you can import the module or specific components into your Fortran source files. For example, use `use datastructs_fortran` to import the entire module, or `use datastructs_fortran, only: [component_name]` to import specific types or functions.

Main derived types and routines:

- Dynamical List (`dynamical_list_t`): Create and manipulate a list that can grow or shrink dynamically. Examples at [example/dynamical_list.f90](./example/dynamical_list.f90) and [example/dynamical_list2.f90](./example/dynamical_list2.f90)

- Fixed List (`fixed_list_t`): Define a fixed-size list, which can be linked to others. Examples at [example/fixed_list.f90](./example/fixed_list.f90)

- Max Heap (`maxheap_t`): Implement a max heap structure, where the parent node is always greater than or equal to its children. Examples at [example/maxheap.f90](./example/maxheap.f90)

- Hash Function (`djb2`): Calculate a hash value for an array of integers using the djb2 algorithm. Examples at [example/hash.f90](./example/hash.f90)

- Sampler Algorithms (`sampler_base_t`, `choose_sampler`): Randomly get indexes proportionally to their weights. Examples at [example/sampler.f90](./example/sampler.f90)

- Logger: Provides logging with configurable verbosity and levels (`LOG_ERROR`, `LOG_WARNING`, `LOG_INFO`, `LOG_DEBUG`); global controls (`LOGGER_VERBOSE`, `LOGGER_LEVEL`, `LOGGER_OUTPUT_UNIT`, `LOGGER_ERROR_UNIT`); main routines for configuring and writing logs: `set_verbose`, `set_level`, `set_output_unit`, `set_error_unit`, `set_unit_defaults`, `log_unit`, `log_write`. Examples at [example/logger.f90](./example/logger.f90)

## Running examples

Use `fpm run --example name`, in which `name` is the name of the example.
