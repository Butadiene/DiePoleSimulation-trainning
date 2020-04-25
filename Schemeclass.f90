

module scheme_class
    implicit none
    type scheme
        private
        double precision dt
        integer timeStep
        contains
            procedure :: 