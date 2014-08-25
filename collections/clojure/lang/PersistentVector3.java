package clojure.lang;

import java.util.Iterator;

public class PersistentVector3 extends APersistentVector implements IObj,
	IEditableCollection, IReduce {
    final Object e0;
    final Object e1;
    final Object e2;
    private final IPersistentMap meta;
    private int hash = -1;
    private int hasheq = -1;

    PersistentVector3(IPersistentMap meta, Object e0, Object e1, Object e2) {
	this.meta = meta;
	this.e0 = e0;
	this.e1 = e1;
	this.e2 = e2;
    }

    public PersistentVector3(Object e0, Object e1, Object e2) {
	this.meta = null;
	this.e0 = e0;
	this.e1 = e1;
	this.e2 = e2;
    }

    public IPersistentMap meta() {
	return meta;
    }

    public IObj withMeta(IPersistentMap meta) {
	return new PersistentVector3(meta, e0, e1, e2);
    }

    public Object nth(int i) {
	switch (i) {
	case 0:
	    return e0;
	case 1:
	    return e1;
	case 2:
	    return e2;
	default:
	    throw new IndexOutOfBoundsException();
	}
    }

    public Object nth(int i, Object notFound) {
	switch (i) {
	case 0:
	    return e0;
	case 1:
	    return e1;
	case 2:
	    return e2;
	default:
	    return notFound;
	}
    }

    public int count() {
	return 3;
    }

    public IPersistentVector empty() {
	return PersistentVector0.EMPTY;
    }

    public IPersistentVector assocN(int i, Object val) {
	switch (i) {
	case 0:
	    return new PersistentVector3(meta, val, e1, e2);
	case 1:
	    return new PersistentVector3(meta, e0, val, e2);
	case 2:
	    return new PersistentVector3(meta, e0, e1, val);
	case 3:
	    return cons(val);
	default:
	    throw new IndexOutOfBoundsException();
	}
    }

    public IPersistentVector cons(Object val) {
	return new PersistentVector4(meta, e0, e1, e2, val);
    }

    public ITransientCollection asTransient() {
	ITransientCollection coll = PersistentVector.EMPTY.asTransient();
	return (ITransientVector) coll.conj(e0).conj(e1).conj(e2);
    }

    public IPersistentVector pop() {
	return new PersistentVector2(meta, e0, e1);
    }

    public Object kvreduce(IFn f, Object init) {
	init = f.invoke(init, 0, e0);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, 1, e1);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, 2, e2);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public Object reduce(IFn f) {
	Object init = e0;
	init = f.invoke(init, e1);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, e2);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public Object reduce(IFn f, Object init) {
	init = f.invoke(init, e0);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, e1);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, e2);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public int hashCode() {
	if (this.hash == -1) {
	    int hash = 1;
	    hash = (31 * hash) + (e0 == null ? 0 : e0.hashCode());
	    hash = (31 * hash) + (e1 == null ? 0 : e1.hashCode());
	    hash = (31 * hash) + (e2 == null ? 0 : e2.hashCode());
	    this.hash = hash;
	}
	return hash;
    }

    public int hasheq() {
	if (this.hasheq == -1) {
	    int hash = 1;
	    hash = (31 * hash) + Util.hasheq(e0);
	    hash = (31 * hash) + Util.hasheq(e1);
	    hash = (31 * hash) + Util.hasheq(e2);
	    hash = Murmur3.mixCollHash(hash, 3);
	    this.hasheq = hash;
	}
	return hasheq;
    }

    public boolean equals(Object o) {
	if (o instanceof PersistentVector3) {
	    return Util.equals(e0, ((PersistentVector3) o).e0)
		    && Util.equals(e1, ((PersistentVector3) o).e1)
		    && Util.equals(e2, ((PersistentVector3) o).e2);
	} else {
	    return super.equals(o);
	}
    }

    public boolean equiv(Object o) {
	if (o instanceof PersistentVector3) {
	    return Util.equiv(e0, ((PersistentVector3) o).e0)
		    && Util.equiv(e1, ((PersistentVector3) o).e1)
		    && Util.equiv(e2, ((PersistentVector3) o).e2);
	} else {
	    return super.equiv(o);
	}
    }

    public Iterator iterator() {
	return new Iterator() {
	    int i = 0;

	    public boolean hasNext() {
		return i < 3;
	    }

	    public Object next() {
		return nth(i++);
	    }

	    public void remove() {
		throw new UnsupportedOperationException();
	    }
	};
    }

    public ISeq seq() {
	return IteratorSeq.create(iterator());
    }
}